package promela_ast

import "go/token"

// Encodes a Promela binary expression
type BinaryExpr struct {
	Pos token.Position
	Lhs Node   // the left operand
	Op  string // the operand (==,<=.>, etc)
	Rhs Node   // the right operand
}

func (b *BinaryExpr) Position() token.Position {
	return b.Pos
}

func (b *BinaryExpr) Print(num_tabs int) string {
	return b.Lhs.Print(num_tabs) + " " +
		b.Op + " " + b.Rhs.Print(num_tabs)
}

func (s *BinaryExpr) Clone() Node {
	s1 := &BinaryExpr{Pos: s.Pos, Lhs: s.Lhs.Clone(), Rhs: s.Rhs.Clone(), Op: s.Op}
	return s1
}
