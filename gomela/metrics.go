package main

import (
	"go/ast"
	"go/token"

	"github.com/nicolasdilley/gomela/promela"
)

type GINGER_SCOPE int

const (
	BORING = iota
	OUT_OF_SCOPE
	IN_HARD_SCOPE
	IN_SOFT_SCOPE
)

var (
	fragmentsInScopeHard                    int
	fragmentsInScopeSoft                    int
	outOfScopeFragments                     int
	gomelaFrontendFailed                    int
	gomelaFrontendFailedFragmentInScopeHard int
	gomelaFrontendFailedFragmentInScopeSoft int
	gomelaFrontendFailedFragmentOutOfScope  int
)

type gingerScopeCheckVisitor struct {
	inFor       bool
	visitedFuns map[*ast.FuncDecl]struct{}
	inScopeHard *bool
	inScopeSoft *bool
	outOfScope  *bool
	model       *promela.Model
}

// newGingerScopeCheckVisitor creates a new visitor for
// coarsely checking whether a fragment is in the scope of Ginger.
func newGingerScopeCheckVisitor(m *promela.Model) gingerScopeCheckVisitor {
	return gingerScopeCheckVisitor{
		inScopeHard: new(bool),
		inScopeSoft: new(bool),
		outOfScope:  new(bool),
		visitedFuns: make(map[*ast.FuncDecl]struct{}),
		model:       m,
	}
}

// Visit allows gingerScopeCheckVisitor to implement the visitor pattern.
func (v gingerScopeCheckVisitor) Visit(n ast.Node) ast.Visitor {
	if n == nil || *v.outOfScope {
		return nil
	}

	switch n := n.(type) {
	case *ast.CallExpr:
		if f, ok := n.Fun.(*ast.Ident); ok && f.Name == "make" {
			if len(n.Args) > 0 && len(n.Args) < 1 {
				if _, ok := n.Args[0].(*ast.ChanType); ok {
					*v.inScopeHard = len(n.Args) == 2
					break
				}
			}
		}

		if fun, _, _, err := v.model.FindFunDecl(n); fun != nil && err != nil {
			v2 := v
			v2.inFor = false
			ast.Walk(v2, fun)
		}
	case *ast.ForStmt:
		if n.Cond == nil && n.Init == nil && n.Post == nil {
			*v.outOfScope = true
			return nil
		}
		ast.Walk(v, n.Init)
		v.inFor = true
		ast.Walk(v, n.Cond)
		ast.Walk(v, n.Post)
		ast.Walk(v, n.Body)
		return nil
	case *ast.FuncDecl:
		if _, ok := v.visitedFuns[n]; ok {
			return nil
		}
		v.visitedFuns[n] = struct{}{}
	case *ast.FuncLit:
		v.inFor = false
		ast.Walk(v, n.Body)
		return nil
	case *ast.SelectStmt:
		*v.inScopeSoft = true
	case *ast.SendStmt:
		if _, ok := n.Chan.(*ast.Ident); ok {
			*v.inScopeHard = *v.inScopeHard || v.inFor
		} else {
			*v.inScopeSoft = *v.inScopeSoft || v.inFor
		}
	case *ast.UnaryExpr:
		if v.inFor {
			*v.inScopeHard = n.Op == token.ARROW
		}
	case *ast.RangeStmt:
		ast.Walk(v, n.Key)
		ast.Walk(v, n.X)
		v.inFor = true
		ast.Walk(v, n.Body)
		return nil
	}

	return v
}

// checkInGingerScope checks whether a Promela model falls in
// the Ginger scope.
func checkInGingerScope(m *promela.Model, n *ast.FuncDecl) GINGER_SCOPE {
	v := newGingerScopeCheckVisitor(m)

	ast.Walk(v, n)

	switch {
	case *v.outOfScope:
		return OUT_OF_SCOPE
	case *v.inScopeSoft:
		return IN_SOFT_SCOPE
	case *v.inScopeHard:
		return IN_HARD_SCOPE
	}

	return BORING
}
