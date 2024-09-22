package lattice

import (
	"fmt"

	"github.com/cs-au-dk/goat/analysis/defs"
	loc "github.com/cs-au-dk/goat/analysis/location"
)

// Checks whether a location is represented by the provided top location.
// A represented top location represents another location if they have
// the same SSA value as an allocation site
func represents(l1 loc.AddressableLocation, l2 loc.Location) bool {
	switch l1 := l1.(type) {
	case loc.AllocationSiteLocation:
		// Allocation sites can be representatives if their
		// denoted goroutine is the top goroutine
		g := l1.Goro.(defs.Goro)
		if l2, ok := l2.(loc.AllocationSiteLocation); ok &&
			g.Equal(defs.Create().TopGoro()) {
			site, ok := l2.GetSite()
			return l1.Site != nil && ok && site == l1.Site
		}
		return false
	default:
		panic(fmt.Sprintf("Location %s is not a representative location", l1))
	}
}

// IsTopLocation checks whether a location is an unkown location. This is the case for
// local or allocation site locations where the owning goroutine is unkown.
func IsTopLocation(l loc.Location) bool {
	switch l := l.(type) {
	case loc.GlobalLocation,
		loc.LocalLocation,
		loc.NilLocation,
		loc.FunctionPointer:
		return false
	case loc.AllocationSiteLocation:
		g := l.Goro.(defs.Goro)
		return defs.Create().TopGoro().Equal(g)
	case loc.FieldLocation:
		// A field location is the top location if its base is the top location.
		return IsTopLocation(l.Base)
	default:
		panic(fmt.Sprintf("Location %s cannot be checked for top", l))
	}
}

// representative produces a representative unknown location from an allocation site location.
func representative(l loc.AddressableLocation) (loc.AllocationSiteLocation, bool) {
	switch l := l.(type) {
	case loc.AllocationSiteLocation:
		if IsTopLocation(l) {
			return l, false
		}

		s, _ := l.GetSite()
		return loc.AllocationSiteLocation{
			Site:    s,
			Goro:    defs.Create().TopGoro(),
			Context: s.Parent(),
		}, true
	default:
		return loc.AllocationSiteLocation{}, false
	}
}

// recRepresentative recursively ceates a representative unknown location for any
// type of location.
func recRepresentative(l loc.Location) (loc.Location, bool) {
	switch l := l.(type) {
	case loc.FieldLocation:
		if rep, hasRep := recRepresentative(l.Base); hasRep {
			l.Base = rep
			return l, true
		}
	case loc.AllocationSiteLocation:
		if rep, hasRep := representative(l); hasRep {
			return rep, true
		}
	}

	return l, false
}
