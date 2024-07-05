package promela

import (
	"errors"
	"go/ast"
	"go/types"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

func (m *Model) translateRcvStmt(
	commCase bool,
	e ast.Expr,
	body *promela_ast.BlockStmt,
	body2 *promela_ast.BlockStmt) (promela_ast.GuardStmt, error) {

	if m.containsChan(e) {
		chan_name := m.getChanStruct(e)
		return &GenRcvStmt{
			IsCommCase: commCase,
			Pos:        m.Props.Fileset.Position(e.Pos()),
			Chan:       chan_name.Name,
			M:          m.Props,
			Sync_body:  body,
			Async_body: body2,
		}, nil
	}

	if !m.IsTimeAfter(e) {
		return nil, errors.New(UNKNOWN_RCV + m.Props.Fileset.Position(e.Pos()).String())
	}

	return &promela_ast.SingleGuardStmt{
		Cond: &promela_ast.Ident{Name: "true"},
		Body: body}, nil
}

func (m *Model) IsTimeAfter(e ast.Expr) bool {
	switch call := e.(type) {
	case *ast.SelectorExpr:
		// Checking if timeout.C channel and change it to a true branch
		switch t := GetElemIfPointer(m.AstMap[m.Package].TypesInfo.TypeOf(call.X)).(type) {
		case *types.Named:
			return t.String() == "time.Ticker" || t.String() == "time.Timer"
		}
	case *ast.CallExpr:
		switch sel := call.Fun.(type) {
		case *ast.SelectorExpr:
			switch ident := sel.X.(type) {
			case *ast.Ident:
				if ident.Name == "time" && sel.Sel.Name == "After" {
					return true
				}
			}

			if sel.Sel.Name != "Done" {
				break
			}
			// look if the type is context.Context
			switch ident := sel.X.(type) {
			case *ast.Ident:
				if ident.Name == "ctx" {
					return true
				}
			}

			switch t := GetElemIfPointer(m.AstMap[m.Package].TypesInfo.TypeOf(sel.X)).(type) {

			case *types.Named:
				return t.String() == "context.Context"
			}
		}
	}

	return false
}
