package promela

import (
	"github.com/nicolasdilley/gomela/promela/promela_ast"

	"go/token"
)

// a Promela send statement is a *guard* statement which either sends its respective monitor or a Promela channel when running in Ginger mode
type GenRcvStmt struct {
	IsCommCase bool
	Pos        token.Position
	Model      string
	M          *GlobalProps     // a pointer to the model to check whether it is running in Ginger mode
	Chan       promela_ast.Node // the chan that we want to send on
	Sync_body  *promela_ast.BlockStmt
	Async_body *promela_ast.BlockStmt
}

func (s *GenRcvStmt) Position() token.Position {
	return s.Pos
}

func (s *GenRcvStmt) Print(num_tabs int) string {

	// if contains close send to monitor

	if s.M.GingerMode {
		rcv := &promela_ast.RcvStmt{
			Chan:  s.Chan,
			Model: "Recv",
			Rhs:   &promela_ast.Ident{Name: "0"},
		}
		var rcv_guard promela_ast.Node = &promela_ast.BlockStmt{
			List: append([]promela_ast.Node{rcv}, s.Sync_body.List...),
		}
		if s.IsCommCase {
			rcv_guard = &promela_ast.SingleGuardStmt{
				Cond: rcv,
				Guard: s.Pos,
				Body: &promela_ast.BlockStmt{
					List: s.Sync_body.List,
				},
			}
		}
		return rcv_guard.Print(num_tabs)
	}

	async_rcv := &promela_ast.RcvStmt{
		Rcv: s.Pos,
		Chan: &promela_ast.SelectorExpr{
			X:   s.Chan,
			Sel: &promela_ast.Ident{Name: "deq"}},
		Rhs: &promela_ast.Ident{Name: "state,num_msgs"}}

	sync_rcv := &promela_ast.RcvStmt{
		Rcv: s.Pos,
		Chan: &promela_ast.SelectorExpr{
			X:   s.Chan,
			Sel: &promela_ast.Ident{Name: "sync"}},
		Rhs: &promela_ast.Ident{Name: "state"}}

	async_guard := &promela_ast.SingleGuardStmt{
		Cond:  async_rcv,
		Guard: s.Pos,
		Body:  s.Async_body}

	sync_guard_body := &promela_ast.BlockStmt{List: []promela_ast.Node{
		&promela_ast.SendStmt{
			Chan: &promela_ast.SelectorExpr{
				X:   s.Chan,
				Sel: &promela_ast.Ident{Name: "rcving"},
			},
			Rhs: &promela_ast.Ident{Name: "false"},
		},
	}}

	sync_guard_body.List = append(sync_guard_body.List, s.Sync_body.List...)
	sync_guard := &promela_ast.SingleGuardStmt{
		Cond: sync_rcv,
		Body: sync_guard_body,
	}
	if s.IsCommCase {
		return async_guard.Print(num_tabs) + "\n" + sync_guard.Print(num_tabs)
	}
	if_stmt := &promela_ast.IfStmt{
		If:   s.Pos,
		Model: "Recv",
		Init: &promela_ast.BlockStmt{
			List: []promela_ast.Node{},
		},
		Guards: []promela_ast.GuardStmt{
			async_guard,
			sync_guard,
		},
	}
	return if_stmt.Print(num_tabs)
}

func (s *GenRcvStmt) Clone() promela_ast.Node {
	s1 := &GenRcvStmt{
		IsCommCase: s.IsCommCase,
		Pos:        s.Pos,
		Chan:       s.Chan.Clone(),
		M:          s.M,
		Model:      s.Model,
		Sync_body:  s.Sync_body.Clone().(*promela_ast.BlockStmt),
		Async_body: s.Async_body.Clone().(*promela_ast.BlockStmt),
	}
	return s1
}

func (s *GenRcvStmt) GetBody() *promela_ast.BlockStmt {

	body := append(s.Sync_body.List, s.Async_body.List...)

	return &promela_ast.BlockStmt{
		List: body,
	}
}
