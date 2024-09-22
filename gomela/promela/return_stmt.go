package promela

import (
	"errors"
	"go/ast"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

func (m *Model) translateReturnStmt(s *ast.ReturnStmt) (b *promela_ast.BlockStmt, defers *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	defers = &promela_ast.BlockStmt{List: []promela_ast.Node{}}
	for _, spec := range s.Results {
		expr, err1 := m.TranslateExpr(spec)
		err = errors.Join(err, err1)

		switch {
		case m.containsChan(spec):
			return b, defers, errors.New(RETURN_CHAN + m.Props.Fileset.Position(spec.Pos()).String())
		case m.containsWaitgroup(spec):
			return b, defers, errors.New(RETURN_WG + m.Props.Fileset.Position(spec.Pos()).String())
		case m.containsMutex(spec):
			return b, defers, errors.New(RETURN_MUTEX + m.Props.Fileset.Position(spec.Pos()).String())
		case m.isStructWithChans(spec):
			return b, defers, errors.New(RETURN_STRUCT + m.Props.Fileset.Position(spec.Pos()).String())
		}

		addBlock(b, expr)
	}

	b.List = append(b.List, &promela_ast.GotoEndStmt{Goto: m.Props.Fileset.Position(s.Pos()), Name: m.Current_return_label})
	return b, defers, err
}
