package promela

import (
	"errors"
	"go/ast"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

func (m *Model) translateDeclStmt(stmt *ast.DeclStmt) (b *promela_ast.BlockStmt, err error) {
	b = &promela_ast.BlockStmt{List: []promela_ast.Node{}}

	switch decl := stmt.Decl.(type) {
	case *ast.GenDecl:
	SPECS:
		for _, spec := range decl.Specs {
			switch spec := spec.(type) {
			case *ast.ValueSpec:
				lhs := make([]ast.Expr, 0, len(spec.Names))
				for _, name := range spec.Names {
					lhs = append(lhs, name)
				}

				b1, err1 := m.translateNewVar(stmt, lhs, spec.Values)
				addBlock(b, b1)
				if err1 != nil {
					err = err1
				}

				var expr ast.Expr = spec.Type
				// check if its the declaration of a WG
				switch sel := spec.Type.(type) {
				case *ast.StarExpr:
					expr = sel.X
				}

				if len(spec.Values) > 0 {
					continue SPECS
				}

				switch sel := expr.(type) { // looking for var wg *sync.Waitgroup;
				case *ast.SelectorExpr:
					if sel.Sel.Name != "WaitGroup" {
						continue SPECS
					}

					switch sel := sel.X.(type) {
					case *ast.Ident:
						if sel.Name != "sync" {
							continue SPECS
						}

						for _, l := range lhs {
							// we have a waitgroup
							b1, err1 := m.translateWg(stmt, l)
							err = errors.Join(err, err1)
							addBlock(b, b1)
						}
					}
				}
			}
		}
	}

	switch d := stmt.Decl.(type) {
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			switch spec := spec.(type) {
			case *ast.ValueSpec:
				for _, val := range spec.Values {
					expr, err1 := m.TranslateExpr(val)
					err = errors.Join(err, err1)
					addBlock(b, expr)
				}
			}
		}
	}
	return b, err
}
