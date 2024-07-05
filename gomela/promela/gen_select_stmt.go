package promela

import (
	"go/token"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

// A select statement. Will be generated differently based on whether running in Ginger mode or not.
type GenSelectStmt struct {
	M     		*GlobalProps
	Pos 		token.Position
	Goto 		*promela_ast.GotoStmt
	Model 		string
	Name  		string
	Guards 		[]promela_ast.GuardStmt
	DefaultCase promela_ast.GuardStmt

}

func (s *GenSelectStmt) Position() token.Position {
	return s.M.Fileset.Position(token.NoPos)
}

func (s *GenSelectStmt) Print(num_tabs int) string {
	if s.M.GingerMode {
		// TODO:
		i := &promela_ast.IfStmt{
			Model: s.Model,
			Guards: s.Guards,
			If: s.Pos,
		}
		if s.DefaultCase != nil {
			i.Guards = append(i.Guards, s.DefaultCase)
		}
		return i.Print(num_tabs)
	}

	guards := make([]promela_ast.GuardStmt, 0, len(s.Guards))
	for _, guard := range s.Guards {
		guardClone := guard.Clone()
		checkForBreak(guardClone, s.Goto)
		guards = append(guards, guardClone.(promela_ast.GuardStmt))
	}
	if s.DefaultCase != nil {
		defCase := s.DefaultCase.Clone()
		checkForBreak(defCase, s.Goto)
		guards = append(guards, defCase.(promela_ast.GuardStmt))
	}

	i := &promela_ast.SelectStmt{
		Select: s.Pos,
		Model: s.Model,
		Guards: guards,
		Has_default: s.DefaultCase != nil,
	}
	return i.Print(num_tabs)
}

func (s *GenSelectStmt) Clone() promela_ast.Node {
	s1 := &GenSelectStmt{
		Pos: s.Pos,
		M: s.M,
		Model: s.Model,
	}
	if s.DefaultCase != nil {
		s1.DefaultCase = s.DefaultCase.Clone().(promela_ast.GuardStmt)
	}

	for _, g := range s.Guards {
		if g != nil {
			s1.Guards = append(s1.Guards, g.Clone().(promela_ast.GuardStmt))
		}
	}
	return s1
}
