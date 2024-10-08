package lattice

import (
	"fmt"

	i "github.com/cs-au-dk/goat/utils/indenter"
)

// FlatLattice is a baseline for extending flat lattices, exposing
// their ⊥ and ⊤ values.
type FlatLattice struct {
	top FlatTop
	bot FlatBot
}

// init initialized the ⊥ and ⊤ values for a given lattice.
func (l *FlatLattice) init(outer Lattice) {
	inner := flatElementBase{element{outer}}
	l.top = FlatTop{inner}
	l.bot = FlatBot{inner}
}

// Top yields the corresponding ⊤ member.
func (l *FlatLattice) Top() Element {
	return l.top
}

// Bot yields the corresponding ⊥ member.
func (l *FlatLattice) Bot() Element {
	return l.bot
}

// ConstantPropagationLattice is a heterogeneously valued lattice for any type of constant.
type ConstantPropagationLattice struct {
	lattice
	FlatLattice
}

var constantPropagationLattice = func() *ConstantPropagationLattice {
	lat := &ConstantPropagationLattice{}
	lat.init(lat)
	return lat
}()

func (latticeFactory) ConstantPropagation() *ConstantPropagationLattice {
	return constantPropagationLattice
}

func (l1 *ConstantPropagationLattice) Eq(l2 Lattice) bool {
	switch l2 := l2.(type) {
	case *ConstantPropagationLattice:
		return true
	case *Lifted:
		return l1.Eq(l2.Lattice)
	case *Dropped:
		return l1.Eq(l2.Lattice)
	default:
		return false
	}
}

func (ConstantPropagationLattice) String() string {
	return colorize.Lattice("Constant")
}

// FlatFiniteLattice is a flat lattice over a finite domain of values.
type FlatFiniteLattice struct {
	lattice
	FlatLattice
	dom map[any]Element
}

// Flat generates a flat lattice with the given non-⊥/⊤ elements.
func (latticeFactory) Flat(dom ...any) *FlatFiniteLattice {
	lat := new(FlatFiniteLattice)
	lat.init(lat)
	lat.dom = make(map[any]Element)
	for _, el := range dom {
		lat.dom[el] = flatElement{
			element{lat},
			el,
		}
	}
	return lat
}

// Flat
func (l *FlatFiniteLattice) FlatFinite() *FlatFiniteLattice {
	return l
}

func (l *FlatFiniteLattice) String() string {
	strs := []fmt.Stringer{}
	for _, el := range l.dom {
		strs = append(strs, el)
	}
	return i.Indenter().Start(colorize.Lattice("⊥")+" < {").
		NestSep(",", strs...).
		End("} < " + colorize.Lattice("T"))
}

func (l1 *FlatFiniteLattice) Eq(l2 Lattice) bool {
	if l1 == l2 {
		return true
	}
	switch l2 := l2.(type) {
	case *FlatFiniteLattice:
		if len(l1.dom) != len(l2.dom) {
			return false
		}
		for e1 := range l1.dom {
			if _, included := l2.dom[e1]; !included {
				return false
			}
		}
		return true
	case *Lifted:
		return l1.Eq(l2.Lattice)
	case *Dropped:
		return l1.Eq(l2.Lattice)
	default:
		return false
	}
}

type FlatIntLattice struct {
	lattice
	FlatLattice
}

var flatIntLattice = func() *FlatIntLattice {
	lat := new(FlatIntLattice)
	lat.init(lat)
	return lat
}()

func (latticeFactory) FlatInt() *FlatIntLattice {
	return flatIntLattice
}

func (l *FlatIntLattice) String() string {
	return colorize.Lattice("⊢ℤ⊣")
}

func (l1 *FlatIntLattice) Eq(l2 Lattice) bool {
	switch l2 := l2.(type) {
	case *FlatIntLattice:
		return true
	case *Lifted:
		return l1.Eq(l2.Lattice)
	case *Dropped:
		return l1.Eq(l2.Lattice)
	default:
		return false
	}
}

func (l *FlatIntLattice) FlatInt() *FlatIntLattice {
	return l
}
