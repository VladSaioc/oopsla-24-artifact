package promela

import (
	"go/ast"
	"go/types"

	"github.com/nicolasdilley/gomela/promela/promela_types"
)

// Converts an expression to a Promela type.
func (m *Model) ExprToPromelaType(e ast.Expr) (t promela_types.Types, ok bool) {
	if m.AstMap == nil {
		return
	}

	p, ok := m.AstMap[m.Package]
	if !ok || p.TypesInfo == nil {
		return
	}


	switch et := p.TypesInfo.TypeOf(e).(type) {
	// Recognizable basic types produce an equivalent basic Promela type.
	case *types.Basic:
		switch et.Kind() {
		// Boolean types produce the Promela bool type.
		case types.Bool, types.UntypedBool:
			return promela_types.Bool, true
		// Integer-like types produce the Promela int type.
		case types.Int, types.Int8, types.Int16, types.Int32, types.Int64,
			types.Uint, types.Uint8, types.Uint16, types.Uint32, types.Uint64,
			types.UntypedInt:
			return promela_types.Int, true
		}
	// Array and slice types produce the int type.
	case *types.Array, *types.Slice:
		return promela_types.Int, true
	}

	return
}
