package promela_ast

import (
	"go/token"
)

// a send statement where chan is the channel that we want to send to and Rhs the expression we want to sent
type SendStmt struct {
	Send  token.Position
	Model string
	Chan  Node // the chan that we want to send on
	Rhs   Node // the value we want to send
}

func (s *SendStmt) Position() token.Position {
	return s.Send
}

func (s *SendStmt) Print(num_tabs int) string {

	comment := ""

	if s.Send.String() != "-" {
		comment = " /* " + s.Model + "\t" + s.Send.String() + " */"
	}
	return s.Chan.Print(num_tabs) + "!" + s.Rhs.Print(num_tabs) + comment
}

func (s *SendStmt) Clone() Node {
	s1 := &SendStmt{Send: s.Send, Chan: s.Chan.Clone(), Rhs: s.Rhs.Clone(), Model: s.Model }
	return s1
}
