package promela

import (
	"github.com/nicolasdilley/gomela/promela/promela_ast"
	"github.com/nicolasdilley/gomela/promela/promela_types"

	"go/token"
)

// A channel parameter is either of the form "chan c", or "Chandef c", based on M.GingerMode.
type GenChanParam struct {
	Pos   token.Position
	Model string
	M     *GlobalProps // a pointer to the model to check whether it's running or not
	Name  string       // the chan that we want to send on
}

func (s *GenChanParam) Position() token.Position {
	return s.Pos
}

func (s *GenChanParam) Print(num_tabs int) string {
	// In ginger mode, produce a Promela channel.
	var p *promela_ast.Param
	if s.M.GingerMode {
		p = &promela_ast.Param{
			Pos:   s.Pos,
			Name:  s.Name,
			Types: promela_types.Chan,
		}
	} else {
		p = &promela_ast.Param{
			Pos:   s.Pos,
			Name:  s.Name,
			Types: promela_types.Chandef,
		}
	}
	return p.Print(num_tabs)
}

func (s *GenChanParam) Clone() promela_ast.Node {
	s1 := &GenChanParam{Pos: s.Pos, Name: s.Name, M: s.M, Model: s.Model}
	return s1
}
