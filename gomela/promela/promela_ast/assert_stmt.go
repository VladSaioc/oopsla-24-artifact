package promela_ast

import "go/token"

// Encodes a Promela assert statement.
type AssertStmt struct {
	Pos   token.Position
	Model string
	Expr  Node // the left operand
}

// Returns the original Go program source location
func (a *AssertStmt) Position() token.Position {
	return a.Pos
}

// Unparses assert statement (with comment to Go source location).
func (a *AssertStmt) Print(num_tabs int) string {
	comment := ""

	if a.Pos.String() != "-" {
		comment = " /* " + a.Model + "\t" + a.Pos.String() + " */"
	}

	return "assert(" + a.Expr.Print(num_tabs) + ")" + comment

}

func (s *AssertStmt) Clone() Node {
	s1 := &AssertStmt{
		Pos: s.Pos,
		Expr: s.Expr.Clone(),
		Model: s.Model,
	}
	return s1
}
