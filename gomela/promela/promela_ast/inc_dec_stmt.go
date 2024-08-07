package promela_ast

import "go/token"

// an increment or decrement statement x++ or a--
type IncDecStmt struct {
	Stmt token.Position
	X    Node   // the expression to increment or decrement
	Op   string // ++ or --
}

func (s *IncDecStmt) Position() token.Position {
	return s.Stmt
}

func (s *IncDecStmt) Print(num_tabs int) string {
	return s.X.Print(num_tabs) + s.Op
}
func (s *IncDecStmt) Clone() Node {
	s1 := &IncDecStmt{Stmt: s.Stmt, X: s.X.Clone(), Op: s.Op}
	return s1
}
