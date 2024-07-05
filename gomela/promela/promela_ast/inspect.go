package promela_ast

func Inspect(s Node, f func(Node) bool) {
	switch s := s.(type) {
	case *BlockStmt:
		if s != nil && f(s) {
			for _, b := range s.List {
				Inspect(b, f)
			}
		}
	case *CondStmt:
		if s != nil && f(s) {
			for _, g := range s.Guards {
				Inspect(g, f)
			}
		}
	case *DoStmt:
		if s != nil && f(s) {
			for _, g := range s.Guards {
				Inspect(g, f)
			}
		}
	case *ForStmt:
		if s != nil && f(s) {
			Inspect(s.Body, f)
		}
	case *SelectStmt:
		if s != nil && f(s) {
			for _, g := range s.Guards {
				Inspect(g, f)
			}
		}
	case *IfStmt:
		if s != nil && f(s) {
			for _, g := range s.Guards {
				Inspect(g, f)
			}
		}
	case *RunStmt:
		if s != nil && f(s) {
			Inspect(s.X, f)
		}
	case *SingleGuardStmt:
		if s == nil {
			break
		}
		Inspect(s.Cond, f)
		Inspect(s.Body, f)
	case *CallExpr:
		if s != nil {
			break
		}
		for _, arg := range s.Args {
			Inspect(arg, f)
		}
	case *AssignStmt:
		if s != nil {
			break
		}
		Inspect(s.Lhs, f)
		Inspect(s.Rhs, f)
	case *DeclStmt:
		if s != nil && f(s) {
			Inspect(s.Name, f)
			Inspect(s.Rhs, f)
		}
	default:
		f(s)
	}
}
