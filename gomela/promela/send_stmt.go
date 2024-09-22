package promela

import (
	"errors"

	"github.com/nicolasdilley/gomela/promela/promela_ast"

	"go/ast"
)

func (m *Model) translateSendStmt(s *ast.SendStmt) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}

	var guard promela_ast.GuardStmt
	guard, err = m.generateGenSendStmt(s.Chan, &promela_ast.BlockStmt{
		List: []promela_ast.Node{},
	}, &promela_ast.BlockStmt{
		List: []promela_ast.Node{},
	})

	expr, err1 := m.TranslateExpr(s.Value)
	err = errors.Join(err, err1)
	addBlock(b, expr)

	b.List = append(b.List, guard)

	return b, err
}

func (m *Model) generateGenSendStmt(e ast.Expr, body *promela_ast.BlockStmt, body2 *promela_ast.BlockStmt) (g promela_ast.GuardStmt, err error) {
	if !m.containsChan(e) {
		err = errors.New(UNKNOWN_SEND + m.Props.Fileset.Position(e.Pos()).String())
		return
	}

	// create GenSend instead
	chan_name := m.getChanStruct(e)
	g = &GenSendStmt{
		IsCommCase: false,
		Send:       m.Props.Fileset.Position(e.Pos()),
		Chan:       chan_name.Name,
		M:          m.Props,
		Sync_body:  body,
		Async_body: body2,
	}

	return g, err
}
