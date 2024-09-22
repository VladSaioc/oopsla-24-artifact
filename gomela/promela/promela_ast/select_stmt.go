package promela_ast

import (
	"go/token"

	"github.com/nicolasdilley/gomela/promela/utils"
)

type SelectStmt struct {
	Select      token.Position
	Model       string
	Guards      []GuardStmt
	Has_default bool
}

func (s *SelectStmt) Position() token.Position {
	return s.Select
}

func (s *SelectStmt) Print(num_tabs int) (stmt string) {

	comment := ""

	if s.Select.String() != "-" {
		comment = " /* " + s.Model + "\t" + s.Select.String() + " */"
	}
	stmt = "do" + comment + "\n"
	for _, guard := range s.Guards {
		stmt += guard.Print(num_tabs) + "\n"
	}
	stmt += utils.GetTabs(num_tabs) + "od"

	return
}

func (s *SelectStmt) Clone() Node {
	s1 := &SelectStmt{
		Select: s.Select,
		Guards: []GuardStmt{},
		Has_default: s.Has_default,
		Model: s.Model,
	}

	for _, g := range s.Guards {
		s1.Guards = append(s1.Guards, g.Clone().(GuardStmt))
	}
	return s1
}
