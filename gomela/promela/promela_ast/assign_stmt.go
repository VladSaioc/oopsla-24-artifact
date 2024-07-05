package promela_ast

import (
	"go/token"
)

// Encodes a Promela assignment statement.
type AssignStmt struct {
	Assign token.Position
	Lhs    Node
	Rhs    Node
}

// Returns the original Go program source location
func (s *AssignStmt) Position() token.Position {
	return s.Assign
}

// Unparses assignment statement.
func (s *AssignStmt) Print(num_tabs int) string {
	if s.Lhs == nil || s.Rhs == nil {
		return ""
	}
	return s.Lhs.Print(num_tabs) + " = " + s.Rhs.Print(num_tabs)
}
func (s *AssignStmt) Clone() Node {
	s1 := &AssignStmt{Assign: s.Assign}
	if s.Lhs != nil {
		s1.Lhs = s.Lhs.Clone()
	}
	if s.Rhs != nil {
		s1.Rhs = s.Rhs.Clone()
	}
	return s1
}
