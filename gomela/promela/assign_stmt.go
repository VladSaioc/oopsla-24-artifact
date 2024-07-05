package promela

import (
	"errors"
	"go/ast"
	"go/token"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

// translateAssignStmt translates a Go assignment statement to Promela.
func (m *Model) translateAssignStmt(s *ast.AssignStmt) (b *promela_ast.BlockStmt, err error) {

	b, err = m.translateNewVar(s, s.Lhs, s.Rhs)
	if err != nil {
		return b, err
	}

	for i, spec := range s.Rhs {
		switch spec := spec.(type) {
		case *ast.FuncLit:
			return b, errors.New(FUNC_DECLARED_AS_VAR + m.Props.Fileset.Position(spec.Pos()).String())
		case *ast.UnaryExpr:
			switch spec.Op {

			case token.ARROW:
				var guard promela_ast.GuardStmt

				guard, err = m.translateRcvStmt(false, spec.X, &promela_ast.BlockStmt{List: []promela_ast.Node{}}, &promela_ast.BlockStmt{List: []promela_ast.Node{}})

				b.List = append(b.List, guard)
				ch := m.getChanStruct(spec.X)
				if len(s.Lhs) != 2 {
					continue
				}
				switch v := s.Lhs[1].(type) {
				case *ast.Ident:
					if v.Name != "_" {
						m.ClosedVars[ch] = append(m.ClosedVars[ch], v)
					}
				case *ast.SelectorExpr:
					m.ClosedVars[ch] = append(m.ClosedVars[ch], v)
				}
			default:
				expr, err1 := m.TranslateExpr(spec.X)
				err = errors.Join(err, err1)
				addBlock(b, expr)
			}
		default:
			expr, err1 := m.TranslateExpr(spec)
			err = errors.Join(err, err1)

			if expr != nil && len(expr.List) > 0 {
				addBlock(b, expr)
				continue
			}

			// Checks if all is known on the right side
			// check if the left-hand side is a comm param
			if !(m.IsExprKnown(spec) && m.IsExprKnown(s.Lhs[i])) {
				continue
			}
			// If it is then translate the assignment as is
			lhs, _ := m.TranslateKnownExpr(s.Lhs[i])
			rhs, comm_pars := m.TranslateKnownExpr(spec)
			stmt := []promela_ast.Node{
				&promela_ast.AssignStmt{Lhs: lhs, Rhs: rhs}}
			// If the assignment is a definition, swap the assignment statement
			// with a declaration, and assign it the type of the right-hand side
			// expression.
			if s.Tok == token.DEFINE {
				ty, ok := m.ExprToPromelaType(spec)
				if ok {
					stmt[0] = &promela_ast.DeclStmt{
						Decl:  m.Props.Fileset.Position(spec.Pos()),
						Name:  lhs.(*promela_ast.Ident),
						Types: ty,
						Rhs:   rhs,
					}
				}
			}

			// flag the lhs as an alias so that its not turned into an unknown comm param
			m.FlagCommParamAsAlias(s.Lhs[i], comm_pars)
			addBlock(b, &promela_ast.BlockStmt{List: stmt})
		}
	}

	return b, err
}
