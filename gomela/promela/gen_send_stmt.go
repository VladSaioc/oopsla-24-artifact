package promela

import (
	"github.com/nicolasdilley/gomela/promela/promela_ast"

	"go/token"
)

// a Promela send statement is a *guard* statement which either sends its respective monitor or promela channel based on whether it is running in Ginger mode
type GenSendStmt struct {
	IsCommCase bool
	Send       token.Position
	Model      string
	M          *GlobalProps     // a pointer to the model to check whether it is running Ginger mode or not
	Chan       promela_ast.Node // the chan that we want to send on
	Sync_body  *promela_ast.BlockStmt
	Async_body *promela_ast.BlockStmt
}

func (s *GenSendStmt) Position() token.Position {
	return s.Send
}

func (s *GenSendStmt) Print(num_tabs int) string {

	// if contains close send to monitor
	if s.M.GingerMode {
		send := &promela_ast.SendStmt{
			Chan:  s.Chan,
			Model: s.Model,
			Rhs:   &promela_ast.Ident{Name: "0"},
		}
		var send_guard promela_ast.Node = &promela_ast.BlockStmt{
			List: append([]promela_ast.Node{send}, s.Sync_body.List...),
		}

		if s.IsCommCase {
			send_guard = &promela_ast.SingleGuardStmt{
				Cond: send,
				Body: &promela_ast.BlockStmt{
					List: s.Sync_body.List,
				},
			}
		}
		return send_guard.Print(num_tabs)
	}

	sync_send := &promela_ast.SendStmt{
		Chan: &promela_ast.SelectorExpr{
			X:   s.Chan,
			Sel: &promela_ast.Ident{Name: "sync"}},
		Rhs:   &promela_ast.Ident{Name: "false"}, // the channel is not closed
		Model: s.Model,
		Send:  s.Send,
	}
	async_send := &promela_ast.RcvStmt{
		Chan: &promela_ast.SelectorExpr{
			X:   s.Chan,
			Sel: &promela_ast.Ident{Name: "enq"}},
		Rhs:   &promela_ast.Ident{Name: "ok"},
		Model: s.Model,
		Rcv:   s.Send,
	}

	assert := &promela_ast.AssertStmt{Pos: s.Send, Expr: &promela_ast.Ident{Name: "ok"}}

	sending_chan := &promela_ast.SelectorExpr{X: s.Chan, Sel: &promela_ast.Ident{Name: "sending"}}

	sync_guard := &promela_ast.SingleGuardStmt{
		Cond:  sync_send,
		Body:  &promela_ast.BlockStmt{List: []promela_ast.Node{}},
		Guard: s.Send}

	sync_guard.Body.List = append([]promela_ast.Node{&promela_ast.RcvStmt{
		Chan: sending_chan,
		Rhs:  &promela_ast.Ident{Name: "ok"}},
		assert}, s.Sync_body.List...)

	async_guard := &promela_ast.SingleGuardStmt{
		Cond: async_send,
		Body: &promela_ast.BlockStmt{List: []promela_ast.Node{}}, Guard: s.Send}

	async_guard.Body.List = append([]promela_ast.Node{assert}, s.Async_body.List...)

	if s.IsCommCase {
		return async_guard.Print(num_tabs) + "\n" + sync_guard.Print(num_tabs)
	}
	if_stmt := &promela_ast.IfStmt{
		If:   s.Send,
		Model: "Send",
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

func (s *GenSendStmt) Clone() promela_ast.Node {
	s1 := &GenSendStmt{
		IsCommCase: s.IsCommCase,
		Send:       s.Send,
		Chan:       s.Chan.Clone(),
		M:          s.M,
		Model:      s.Model,
		Sync_body:  s.Sync_body.Clone().(*promela_ast.BlockStmt),
		Async_body: s.Async_body.Clone().(*promela_ast.BlockStmt),
	}
	return s1
}

func (s *GenSendStmt) GetBody() *promela_ast.BlockStmt {

	body := append(s.Sync_body.List, s.Async_body.List...)

	return &promela_ast.BlockStmt{
		List: body,
	}
}
