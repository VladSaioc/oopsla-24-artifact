package promela_ast

import "go/token"

// a receive statement where chan is the channel that we want to receive from and Rhs the var that we will assign the valued received
type RcvStmt struct {
	Rcv   token.Position
	Model string
	Chan  Node // the chan that we want to receive on
	Rhs   Node // the var where we are going to put the value received or a match of a particular value
}

func (s *RcvStmt) Position() token.Position {
	return s.Rcv
}

func (s *RcvStmt) Print(num_tabs int) string {
	comment := ""

	if s.Rcv.String() != "-" {
		comment = " /* " + s.Model + "\t" + s.Rcv.String() + " */"
	}
	return s.Chan.Print(num_tabs) + "?" + s.Rhs.Print(num_tabs) + comment
}
func (s *RcvStmt) Clone() Node {
	s1 := &RcvStmt{
		Rcv: s.Rcv,
		Chan: s.Chan.Clone(),
		Rhs: s.Rhs.Clone(),
		Model: s.Model,
	}
	return s1
}
