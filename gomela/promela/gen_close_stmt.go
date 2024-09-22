package promela

import (
	"github.com/nicolasdilley/gomela/promela/promela_ast"

	"go/token"
)

// a Promela close statement is a *guard* statement which either sends its respective monitor or a Promela channel when running in Ginger mode
type GenCloseStmt struct {
	Pos        token.Position
	Model      string
	M          *GlobalProps     // a pointer to the model to check whether it is running in Ginger mode
	Chan       *ChanStruct // the chan that we want to close
}

func (s *GenCloseStmt) Position() token.Position {
	return s.Pos
}

func (s *GenCloseStmt) Print(num_tabs int) string {

	// if contains close send to monitor
	if s.M.GingerMode {
		return "run close(" + s.Chan.Name.Name + ")\n"
	}

	rcv := &promela_ast.RcvStmt{Model: "Close", Rcv: s.Pos}

	rcv.Chan = &promela_ast.SelectorExpr{
		X: s.Chan.Name, Sel: &promela_ast.Ident{Name: "closing"},
		Pos: s.Pos,
	}
	rcv.Rhs = &promela_ast.Ident{Name: "closed"}

	assert := &promela_ast.AssertStmt{
		Model: "Close",
		Pos:   s.Pos,
		Expr:  &promela_ast.Ident{Name: "!closed"},
	}
	stmt := rcv.Print(num_tabs) + "\n" + assert.Print(num_tabs)
	return stmt
}

func (s *GenCloseStmt) Clone() promela_ast.Node {
	s1 := &GenCloseStmt{
		Pos:        s.Pos,
		Chan:       s.Chan,
		M:          s.M,
		Model:      s.Model,
	}
	return s1
}
