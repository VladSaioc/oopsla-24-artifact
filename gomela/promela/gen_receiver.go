package promela

import (
	"go/token"

	"github.com/nicolasdilley/gomela/promela/promela_ast"
)

// A function to "receiver". Will be generated when synchronous function calls exist in the program.
type GenReceiver struct {
	M     *GlobalProps
	Name  string
	Model string
}

func (s *GenReceiver) Position() token.Position {
	return s.M.Fileset.Position(token.NoPos)
}

func (s *GenReceiver) Print(num_tabs int) string {

	// if contains close send to monitor
	// var p *promela_ast.Param
	if s.M.ContainsReceiver {
		p := &promela_ast.RunStmt{
			X: &promela_ast.CallExpr{
				Fun: &promela_ast.Ident{
					Name: "receiver"},
				Args: []promela_ast.Node{&promela_ast.Ident{Name: s.Name}}}}
		return p.Print(num_tabs)
	}
	return ""
}

func (s *GenReceiver) Clone() promela_ast.Node {
	s1 := &GenReceiver{Name: s.Name, M: s.M, Model: s.Model}
	return s1
}
