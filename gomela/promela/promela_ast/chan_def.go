package promela_ast

import (
	"fmt"
	"go/token"

	"github.com/nicolasdilley/gomela/promela/promela_types"
)

type Chandef struct {
	Name  *Ident
	Def   token.Position
	Size  *Ident
	Types []promela_types.Types
}

func (c *Chandef) Position() token.Position {
	return c.Def
}

func (c *Chandef) Print(num_tabs int) (stmt string) {
	stmt += "chan " + c.Name.Print(num_tabs) + " = ["
	stmt += fmt.Sprintf("%s] of {", c.Size.Name)

	for i, types := range c.Types {
		stmt += types.Name
		if i < len(c.Types)-1 {
			stmt += ","
		}
	}

	stmt += "}"
	return
}

func (s *Chandef) Clone() Node {
	s1 := &Chandef{Def: s.Def, Name: s.Name.Clone().(*Ident), Size: s.Size.Clone().(*Ident), Types: s.Types}
	return s1
}
