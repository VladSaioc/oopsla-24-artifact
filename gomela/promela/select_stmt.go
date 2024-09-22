package promela

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

func (m *Model) translateSelectStmt(s *ast.SelectStmt) (b *promela_ast.BlockStmt, defers *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	defers = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	i := &GenSelectStmt{
		M: m.Props,
		Model: "Select",
		Pos: m.Props.Fileset.Position(s.Pos()),
	}

	was_in_for := m.For_counter.In_for //used to check if this is the outer loop
	had_go := m.For_counter.With_go
	// update 'for' counter to generate the appropriate label
	m.For_counter.With_go = false

	if was_in_for {
		m.For_counter.Y += 1
	} else {
		m.For_counter.X += 1
	}

	defer func() {
		m.For_counter.In_for = was_in_for
		m.For_counter.With_go = had_go
		if !was_in_for { // if outer loop set in for to false and reset y
			m.For_counter.Y = 0
		}
	}()

	i.Goto = &promela_ast.GotoStmt{
		Label: &promela_ast.LabelStmt{
			Name: fmt.Sprintf("for%d%d_exit", m.For_counter.X, m.For_counter.Y),
		},
	}
	goto_end_stmt := &promela_ast.GotoStmt{Label: &promela_ast.LabelStmt{Name: fmt.Sprintf("for%d%d_end", m.For_counter.X, m.For_counter.Y)}}

	for _, comm := range s.Body.List {
		comm, ok := comm.(*ast.CommClause) // can only be a commClause
		if !ok {
			return nil, nil, errors.New("Select case was not *ast.CommClause: " +  m.Props.Fileset.Position(s.Pos()).String())
		}
		body, d1, err1 := m.TranslateBlockStmt(&ast.BlockStmt{List: comm.Body})

		if len(d1.List) > 0 {
			return b, d1, errors.New(DEFER_IN_SELECT + m.Props.Fileset.Position(s.Pos()).String())
		}
		if err1 != nil {
			return b, defers, err1
		}

		// check if default select
		if comm.Comm == nil {
			// it is default
			i.DefaultCase = &promela_ast.SingleGuardStmt{
				Cond: &promela_ast.Ident{
					Name: "default",
				},
				Guard: m.Props.Fileset.Position(comm.Pos()),
				Body: body,
			}
			continue
		}

		// turns of the printing for the survey on the second translation of the body
		m.GenerateFeatures = false
		body2, _, _ := m.TranslateBlockStmt(&ast.BlockStmt{List: comm.Body})
		m.GenerateFeatures = true

		switch com := comm.Comm.(type) {
		case *ast.SendStmt: // send
			if !m.containsChan(com.Chan) {
				err = errors.New(UNKNOWN_SEND + m.Props.Fileset.Position(com.Chan.Pos()).String())
				return b, defers, err
			}

			chan_name := m.getChanStruct(com.Chan)

			gen_send := &GenSendStmt{
				IsCommCase: true,
				Send:       m.Props.Fileset.Position(s.Pos()),
				Chan:       chan_name.Name,
				M:          m.Props,
				Sync_body:  body,
				Async_body: body2,
			}
			i.Guards = append(i.Guards, gen_send)

		case *ast.AssignStmt: //receive
			for _, rh := range com.Rhs {
				var guard promela_ast.GuardStmt
				switch com := rh.(type) {
				case *ast.UnaryExpr:
					if com.Op != token.ARROW {
						continue
					}
					guard, err = m.translateRcvStmt(true, com.X, body, body2)
					i.Guards = append(i.Guards, guard)
				case *ast.Ident:
					guard, err = m.translateRcvStmt(true, com, body, body2)
					i.Guards = append(i.Guards, guard)
				case *ast.SelectorExpr:
					guard, err = m.translateRcvStmt(true, com, body, body2)
					i.Guards = append(i.Guards, guard)
				}
			}
		case *ast.ExprStmt:
			comm, ok := com.X.(*ast.UnaryExpr)
			if !ok || comm.Op != token.ARROW {
				continue
			}
			var guard promela_ast.GuardStmt
			guard, err = m.translateRcvStmt(true, comm.X, body, body2)
			i.Guards = append(i.Guards, guard)
		}
		if err != nil {
			return b, defers, err
		}
	}

	if len(i.Guards) > 0 {
		b.List = append(b.List, i, i.Goto.Label, goto_end_stmt.Label)
	} else {
		return nil, nil, errors.New(SELECT_WITH_NO_BRANCH + m.Props.Fileset.Position(s.Pos()).String())

	}

	return b, defers, err
}
