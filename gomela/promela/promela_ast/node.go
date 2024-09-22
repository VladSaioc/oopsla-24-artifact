package promela_ast

import "go/token"

// Node is implemented by all Promela AST nodes.
type Node interface {
	// Retrieves the position in the original Go program
	Position() token.Position
	// Unparses the AST and pretty prints it at the depth level given by tabs.
	Print(tabs int) string
	// Produces a deep copy of the node.
	Clone() Node
}
