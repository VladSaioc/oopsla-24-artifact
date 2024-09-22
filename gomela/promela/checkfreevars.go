package promela

import (
	"errors"
	"go/ast"
)

// CheckFreeVars checks that all concurrency primitives in the models are not used
// as free variables.
func (m *Model) checkFreeVars(f *ast.FuncLit) (err error) {
	err = nil
	ast.Inspect(f, func(n ast.Node) bool {
		switch n := n.(type) {
		case ast.Expr:
			switch n.(type) {
			case *ast.Ident, *ast.SelectorExpr:
				if m.isChan(n) || m.isWaitgroup(n) || m.isMutex(n) {
					err = errors.New(FUNC_LIT_CONC_PRIM + m.Props.Fileset.Position(n.Pos()).String())
					return false
				}
			}
		}
		return true
	})

	return err
}
