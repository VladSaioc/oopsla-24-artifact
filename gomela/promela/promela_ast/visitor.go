package promela_ast

import "fmt"

// A Visitor's Visit method is invoked for each node encountered by Walk.
// If the result visitor w is not nil, Walk visits each of the children
// of node with the visitor w, followed by a call of w.Visit(nil).
type Visitor interface {
	Visit(node Node) (w Visitor)
}

// Helper functions for common node lists. They may be empty.
func walkList[T Node](v Visitor, list []T) {
	for _, x := range list {
		Walk(v, x)
	}
}

func Walk(v Visitor, node Node) {
	if v = v.Visit(node); v == nil {
		return
	}

	// walk children
	// (the order of the cases matches the order
	// of the corresponding node types in ast.go)
	switch n := node.(type) {
	case *AssertStmt:
		Walk(v, n.Expr)
	case *AssignStmt:
		Walk(v, n.Lhs)
		Walk(v, n.Rhs)
	case *BinaryExpr:
		Walk(v, n.Lhs)
		Walk(v, n.Rhs)
	case *BlockStmt:
		walkList(v, n.List)
	case *CallExpr:
		Walk(v, n.Fun)
		walkList(v, n.Args)
	case *Chandef:
		Walk(v, n.Name)
		Walk(v, n.Size)
	case *ChanStructDef:
		Walk(v, n.Name)
		walkList(v, n.Defs)
		walkList(v, n.Decls)
	case *CommParamDeclStmt:
		Walk(v, n.Name)
		Walk(v, n.Rhs)
	case *CondStmt:
		walkList(v, n.Guards)
	case *DeclStmt:
		Walk(v, n.Name)
		Walk(v, n.Rhs)
	case *DefineStmt:
		Walk(v, n.Name)
		Walk(v, n.Rhs)
	case *DoStmt:
		walkList(v, n.Guards)
	case *ExprStmt:
		Walk(v, n.X)
	case *ForStmt:
		Walk(v, n.Lb)
		Walk(v, n.Ub)
		Walk(v, n.Body)
	case *IncDecStmt:
		Walk(v, n.X)
	case *IfStmt:
		if n.Init != nil {
			Walk(v, n.Init)
		}
		walkList(v, n.Guards)
	case *LabelStmt, *Ident:
	case *SelectorExpr:
		Walk(v, n.X)
		Walk(v, n.Sel)
	case *SelectStmt:
		walkList(v, n.Guards)
	case *SendStmt:
		Walk(v, n.Chan)
		Walk(v, n.Rhs)
	default:
		panic(fmt.Sprintf("promela_ast.Walk: unexpected node type %T", n))
	}

	v.Visit(nil)
}
