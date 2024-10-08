package absint

import (
	"fmt"
	T "go/types"
	"log"
	"strings"

	"github.com/cs-au-dk/goat/analysis/absint/ops"
	"github.com/cs-au-dk/goat/analysis/cfg"
	"github.com/cs-au-dk/goat/analysis/defs"
	L "github.com/cs-au-dk/goat/analysis/lattice"
	loc "github.com/cs-au-dk/goat/analysis/location"
	"github.com/cs-au-dk/goat/utils"
	"github.com/cs-au-dk/goat/utils/graph"

	"golang.org/x/tools/go/ssa"
)

// Blacklisted checks whether a call instruction/callee pair is blacklisted, and should be skipped.
func (C AnalysisCtxt) Blacklisted(callIns ssa.CallInstruction, sfun *ssa.Function) bool {
	// Spoof the call if it is has no SSA instructions.
	// This typically occurs for external functions.
	if len(sfun.Blocks) == 0 {
		return true
	}

	// Skip functions in the testing package, as they perform
	// a lot of locking that we do not want to analyze.
	if pkg := sfun.Pkg; pkg != nil && pkg.Pkg.Name() == "testing" {
		// || pkgName == "net"
		return true
	}

	// Skip analyzing the function bodies of standard library concurrency primitives.
	// We use special abstract values for them, so we cannot abstractly interpret the bodies
	// of their methods on those types.
	if recv := sfun.Signature.Recv(); recv != nil && !opts.SkipSync() &&
		utils.IsModelledConcurrentAPIType(recv.Type()) {
		return true
	}

	// Consult the analysis context to check if the function is included in the fragment.
	return !C.FragmentPredicate(callIns, sfun)
}

// TopInjectParams takes a call instruction, abstract state, and set of blacklisted functions,
// and produces the abstract memory approximating the state after the call returns.
// The memory is updated, such that all heap locations transitively reachable through the parameter's
// values that may have potential side effects are updated to the corresponding ⊤ values for
// their types.
func (C AnalysisCtxt) TopInjectParams(
	callIns ssa.CallInstruction,
	g defs.Goro,
	state L.AnalysisState,
	blacklists map[*ssa.Function]struct{}) L.Memory {
	// Blacklisted functions may involve side-effects on pointer-like arguments.
	// All locations that are members of points-to sets of the arguments
	// must be top injected to over-approximate potential side effects.
	mops := L.MemOps(state.Memory())
	visited := map[loc.Location]bool{}

	// Extract results of the side-effect analysis.
	sideffects := C.LoadRes.WrittenFields

	// Combine side-effect information analysis into a single
	// `affected-by-side-effects` function.
	mapEffects := sideffects.MapCombinedInfo(blacklists)
	sliceEffects := sideffects.SliceCombinedInfo(blacklists)
	pointerEffects := sideffects.PointerCombinedInfo(blacklists)

	var rec func(L.AbstractValue)
	rec = func(v L.AbstractValue) {
		switch {
		case v.IsPointer():
			C.CheckPointsTo(v.PointerValue())
			v.PointerValue().ForEach(func(l loc.Location) {
				if _, isFunc := l.(loc.FunctionPointer); !(isFunc ||
					l.Equal(loc.NilLocation{}) ||
					visited[l]) {
					visited[l] = true
					av := mops.GetUnsafe(l)

					rec(av)

					// Values inside closures cannot be manipulated
					// We do not need to update ⊤ locations.
					if av.IsClosure() || L.IsTopLocation(l) {
						return
					}

					if _, isItf := l.Type().Underlying().(*T.Interface); isItf {
						// Values inside interfaces cannot be manipulated, like closures
						return
					}

					aL := ops.GetAllocationSiteLocation(l)

					updated := false
					if site, found := aL.GetSite(); found {
						switch {
						case C.FocusedPrimitives != nil && C.IsPrimitiveFocused(site):
							// NOTE (Unsound): Skip focused primitives when injecting top
							// TODO: We still recurse on channel payloads. Is this desired?
							// If we choose to not recurse, we have to implement something
							// else to be sound in the case of mutexes that are identified
							// by struct allocation sites (so we recurse on the other fields).
							return
						case av.IsPointer() && !pointerEffects(site) ||
							av.IsMap() && !mapEffects(site) ||
							av.IsArray() && !sliceEffects(site):
							// If no side effects are registered on a collection, do not
							// update the abstract value.
							updated = true
						}
					}

					// Update unknown structs
					// FIXME: This code does not handle pointers to embedded structs correctly.
					//  The written fields analysis currently only says something about which
					//  fields are written to on the outer struct.
					if av.IsKnownStruct() {
						// If the site is not a pointer, proceed normally
						if ptT, ok := l.Type().Underlying().(*T.Pointer); ok {
							// If the site is not a pointer to a struct, proceed normally
							if structT, ok := ptT.Elem().Underlying().(*T.Struct); ok {
								// For every possible blacklisted function, compute which fields
								// they may write to
								isWritten := sideffects.FieldInfo(structT, blacklists)
								// If the value is a known structure, then don't update any fields that
								// may not be overwritten in the function call.
								sv := av.StructValue()
								av.ForEachField(func(i interface{}, av L.AbstractValue) {
									index, ok := i.(int)
									if ok && isWritten(index) {
										sv = sv.Update(i, av.ToTop())
									}
								})

								mops.Update(l, av.Update(sv))
								updated = true
							}
						}
					}

					if !updated {
						//log.Println(callIns, l, "from", av)
						mops.Update(l, av.ToTop())
					}
				}
			})
		case v.IsKnownStruct():
			v.ForEachField(func(i interface{}, v L.AbstractValue) {
				rec(v)
			})
		case v.IsChan():
			rec(v.ChanValue().Payload())
		case v.IsCond() && v.CondValue().IsLockerKnown():
			rec(Elements().AbstractPointerV().UpdatePointer(v.CondValue().KnownLockers()))
		}
	}

	for _, arg := range callIns.Common().Args {
		rec(EvaluateSSA(g, mops.Memory(), arg))
	}

	return mops.Memory()
}

// callSuccs retrieves all the possible successors of a call instruction,
// and the associated abstract states.
func (C AnalysisCtxt) callSuccs(
	sl defs.Superloc,
	g defs.Goro,
	cl defs.CtrLoc,
	state L.AnalysisState) L.AnalysisIntraprocess {
	n := cl.Node()
	succs := Elements().AnalysisIntraprocess()

	var callIns ssa.CallInstruction
	switch n := n.(type) {
	case *cfg.DeferCall:
		callIns = n.DeferLink().(*cfg.SSANode).Instruction().(*ssa.Defer)

	case *cfg.SSANode:
		if call, ok := n.Instruction().(*ssa.Call); ok {
			callIns = call
		}
	}

	if callIns == nil {
		panic(fmt.Errorf("callSuccs of %T %v is not supported", n, n))
	}

	postCall := n.CallRelationNode()

	// A call node might miss a post-call node if the Andersen pointer analysis knows
	// that the receiver is nil. I.e. if the call is guaranteed to panic.
	if postCall != nil {
		// First check for skippable method invocations
		if callIns.Common().IsInvoke() {
			mem, hasModel := C.stdInvoke(g, callIns, state.Memory())
			if hasModel {
				// Skip call relation node to avoid single-silent handling of post-call node
				succs = succs.Update(cl.CallRelationNode().Successor(), state.UpdateMemory(mem))
				return succs
			}
		}
	}

	paramTransfers, mayPanic := C.transferParams(sl, *callIns.Common(), g, g, state.Memory())

	if mayPanic {
		succs = succs.Update(cl.Panic(), state)
	}

	if C.Metrics.Enabled() {
		calleeSet := make(map[*ssa.Function]struct{})

		for fun := range paramTransfers {
			calleeSet[fun] = struct{}{}
		}

		C.Metrics.AddCallees(callIns, calleeSet)
	}

	// TODO: Check that the callees computed by transferParams is a subset of
	// those available in the CFG.

	expandedFunctions := make(map[*ssa.Function]struct{})

	blacklists := make(map[*ssa.Function]struct{})
	for succ := range cl.Successors() {
		if _, isWaiting := succ.Node().(*cfg.Waiting); isWaiting {
			// (*sync.Cond).Wait is wired to a Waiting node instead of the
			// entry of the Wait-function, and will therefore not be in the
			// paramTransfers map. We special-case this here.

			// FIXME: Hacky workaround
			blacklists[nil] = struct{}{}
			continue
		}

		sfun := succ.Node().Function()

		newMem, found := paramTransfers[sfun]
		if !found {
			// Skip any kind of handling for calls that the abstract
			// interpreter knows cannot occur.
			continue
		}

		// If we have a "model" for the called function, use that.
		if nsuccs, hasModel := C.stdCall(sl, g, cl, callIns, state, sfun); hasModel {
			succs = succs.MonoJoin(nsuccs)
		} else if C.Blacklisted(callIns, sfun) {
			blacklists[sfun] = struct{}{}
		} else {
			C.Metrics.ExpandFunction(sfun)
			// Clear the exiting flag when entering a function
			funEntryCl := succ.WithExiting(false)
			// NOTE: Assumes that FunctionEntry defer link goes to FunctionExit
			funExitCl := funEntryCl.Derive(funEntryCl.Node().DeferLink())
			succs = succs.Update(
				funEntryCl,
				// Add charged return edge from function exit to postcall node
				state.UpdateMemory(newMem).AddCharge(
					g, funExitCl, cl.Derive(postCall),
				).AddCharge(
					g, funExitCl.WithExiting(true), cl.Derive(postCall).WithExiting(true),
				),
			)
			expandedFunctions[sfun] = struct{}{}
		}
	}

	if len(blacklists) > 0 {
		// Spoof the call if it may be blacklisted
		// Top-inject parameters of the call to account for stateful side-effects
		mem := C.TopInjectParams(callIns, g, state, blacklists)

		succs = succs.WeakUpdate(
			cl.Derive(postCall),
			state.UpdateMemory(
				// Spoof call by top-injecting the return value location
				spoofCall(g, callIns, mem)),
		)
	}

	if C.Log.Enabled && len(expandedFunctions) > 3 {
		log.Println("Expanded", len(expandedFunctions), "at thread", g, ":", cl)
		for f := range expandedFunctions {
			fmt.Println(utils.SSAFunString(f))
		}
	}

	return succs
}

// For each possible called function, returns a memory where parameters have been moved from the
// caller into the memory of the callee.
// Uses points-to values to determine the possible called functions.
func (C AnalysisCtxt) transferParams(
	sl defs.Superloc,
	call ssa.CallCommon,
	fromG, toG defs.Goro,
	initMem L.Memory,
) (res map[*ssa.Function]L.Memory, mayPanic bool) {
	res = make(map[*ssa.Function]L.Memory)

	if _, ok := call.Value.(*ssa.Builtin); ok {
		// We are transferring parameters into a goroutine started with a direct call to a builtin:
		// go println(i)
		// This needs to be handled differently.
		// We transfer ssa register values from the first to the second goroutine.
		newMem := initMem
		for _, arg := range call.Args {
			// Skip constants, they don't need to be transferred (and they don't have a location)
			if _, ok := arg.(*ssa.Const); !ok {
				newMem = newMem.Update(
					loc.LocationFromSSAValue(toG, arg),
					EvaluateSSA(fromG, initMem, arg),
				)
			}
		}

		// A bit hacky
		res[nil] = newMem
		return
	}

	// Pre-evaluate arguments
	var aArgs []L.AbstractValue
	for _, ssaVal := range call.Args {
		aArgs = append(aArgs, EvaluateSSA(fromG, initMem, ssaVal))
	}

	type callTarget struct {
		closure L.InfiniteMap[any]
		args    []L.AbstractValue
	}

	v, mem := C.swapWildcard(sl, fromG, initMem, call.Value)
	initMem = mem
	bases := v.PointerValue().FilterNilCB(func() { mayPanic = true })
	C.CheckPointsTo(bases)
	targets := map[*ssa.Function]callTarget{}

	if call.IsInvoke() {
		prog := C.LoadRes.Prog

		for _, ptr := range bases.Entries() {
			aloc, ok := ptr.(loc.AllocationSiteLocation)
			if !ok {
				panic(fmt.Errorf("Pointer in invoke was not AllocationSiteLocation: %v %T", ptr, ptr))
			}

			var fun *ssa.Function
			if v, ok := aloc.Site.(ssa.CallInstruction); ok &&
				// Special handling for RLocker model...
				v.Common().Value.Name() == "RLocker" {

				rlockerFunc := v.Common().StaticCallee()
				typ := rlockerFunc.Pkg.Type("rlocker")
				fun = prog.LookupMethod(T.NewPointer(typ.Type()), call.Method.Pkg(), call.Method.Name())

			} else {
				makeItf, ok := aloc.Site.(*ssa.MakeInterface)
				if !ok {
					log.Fatalf("AllocationSiteLocation (%v) did not come from a MakeInterface instruction? %T",
						aloc, aloc.Site)
				}

				fun = prog.LookupMethod(makeItf.X.Type(), call.Method.Pkg(), call.Method.Name())
			}

			receiver := initMem.GetUnsafe(aloc)

			if tar, exists := targets[fun]; exists {
				// Join receiver with existing receivers
				tar.args[0] = tar.args[0].MonoJoin(receiver)
			} else {
				targets[fun] = callTarget{
					args: append([]L.AbstractValue{receiver}, aArgs...),
				}
			}
		}
	} else {
		for _, ptr := range bases.Entries() {
			switch ptr := ptr.(type) {
			case loc.FunctionPointer:
				targets[ptr.Fun] = callTarget{args: aArgs}

			case loc.AddressableLocation:
				closure := initMem.GetUnsafe(ptr)
				targets[closure.Closure()] = callTarget{
					closure.StructValue(),
					aArgs,
				}

			default:
				log.Fatalln(ptr, bases, "???")
			}
		}
	}

	if len(targets) == 0 && !mayPanic {
		panic(fmt.Errorf("no targets computed for call: %s with recv/value %s",
			call.String(),
			EvaluateSSA(fromG, initMem, call.Value)))
	}

	for fun, target := range targets {
		newMem := initMem
		for i, argv := range target.args {
			newMem = newMem.Update(
				loc.LocationFromSSAValue(toG, fun.Params[i]),
				argv,
			)
		}

		if len(fun.FreeVars) != 0 {
			for i, fv := range fun.FreeVars {
				newMem = newMem.Update(
					loc.LocationFromSSAValue(toG, fv),
					target.closure.Get(i).AbstractValue(),
				)
			}
		}

		res[fun] = newMem
	}

	return
}

// Used to make graphs in FunctionExit handling.
type ctrlLocMapper map[defs.CtrLoc]any

func (m ctrlLocMapper) Set(key defs.CtrLoc, value any) { m[key] = value }
func (m ctrlLocMapper) Get(key defs.CtrLoc) (any, bool) {
	v, ok := m[key]
	return v, ok
}

func CtrLocMapper() graph.Mapper[defs.CtrLoc] { return ctrlLocMapper(map[defs.CtrLoc]any{}) }

func (C AnalysisCtxt) exitSuccs(
	g defs.Goro,
	cl defs.CtrLoc,
	initState L.AnalysisState,
) L.AnalysisIntraprocess {
	succs := Elements().AnalysisIntraprocess()
	initMem := initState.Memory()

	n := cl.Node()
	if _, ok := n.(*cfg.FunctionExit); !ok {
		panic(fmt.Errorf("exitSuccs of %T %v is not supported", n, n))
	}

	// Only propagate control to charged successors.
	chargedReturns, _ := initState.ThreadCharges().Get(g)
	returnEdges := chargedReturns.Edges(cl)

	// Safety check
	if len(returnEdges) == 0 && n.Function() != cl.Root() && !cl.Panicked() {
		var buf []string
		for succ := range cl.Successors() {
			buf = append(buf, fmt.Sprintf("%s: %s (%s)", succ.Node().Function(), succ, succ.PosString()))
		}
		log.Fatalf(
			"Did not find any charged sites to return to for\n%s (%s)\nPre-analysis return sites:\n%s",
			cl,
			cl.PosString(),
			strings.Join(buf, "\n"),
		)
	}

	callDAG := C.LoadRes.CallDAG
	exitedComponent := callDAG.ComponentOf(n.Function())

	retState := initState

	/* TODO
	   Abstract GC is disabled because `canGC` is expensive and because
	   it's hard to judge whether the reduced memory size outweighs the
	   benefit of not modifying the memory tree structure.
	if canGC(g, n) {
		retState = initState.UpdateMemory(abstractGC(g, n.Function(), initMem))
	}
	*/

	// NOTE: We cannot use GetUnsafe because goroutines spawned on builtins
	// get wired up as `builtin -> functionexit` (to trigger goroutine termination).
	// We instead assert that a return value exists when we actually need it.
	returnVal, hasReturnVal := initMem.Get(loc.ReturnLocation(g, n.Function()))

	for _, succ := range returnEdges {
		updatedRetState := retState

		// If we are returning out of a component we can remove charged return
		// edges for functions that cannot be on the call stack any more.
		// This includes the charged return edge that we are following in this
		// iteration of the loop, but it may also include other charged return
		// edges. Consider:
		//       >bar
		//      /     \
		//  foo        >qux
		//      \     /
		//       >baz
		// foo calls both bar and baz and both of those call qux.
		// When qux is on the call stack we don't know if bar or baz is on the
		// call stack (we have no context sensitivity to discern those cases)
		// so our over-approximation says that both may be on the call stack.
		// When we return from qux to baz, we must remove the charged return
		// edges: qux -> baz, qux -> bar, bar -> foo, as it is now impossible
		// for bar and qux to be on the call stack. There are some
		// StaticAnalysis tests showing why it is necessary to remove all of
		// those edges.
		// This optimisation does not remove butterfly cycles or otherwise
		// improve precision inside a single intraprocessual fixpoint
		// computation, as we will end up joining the sets of charged return
		// edges at function entry if a function is called multiple times.
		// However, if two calls to the same function are separated by a
		// communication operation in a synchronizing configuration, we will
		// have thrown away the state at function entry between the two
		// fixpoint computations, preventing the join of sets of charged return
		// edges.
		// Deciding which return edges to remove is currently done by performing
		// a graph traversal from the returned to function to discover reachable
		// charged return edges. Edges that are not reached in this traversal
		// are thrown away.
		if callDAG.ComponentOf(succ.Node().Function()) != exitedComponent {
			reachesRoot := false
			reachableExits := map[defs.CtrLoc]bool{}
			graph.Of(CtrLocMapper, func(node defs.CtrLoc) []defs.CtrLoc {
				// The graph contains FunctionExit nodes in the same component that
				// can be reached by following charged return edges.
				// TODO: Think about whether it is necessary to also consider
				// setting the exiting flag if it isn't already.
				_, exit := C.LoadRes.Cfg.FunIO(node.Node().Function())
				// assert edge is not hit
				if cl.Node() == exit {
					log.Fatalln(
						"unexpected cycle. returning from",
						cl, "to", succ,
						callDAG.ComponentOf(cl.Node().Function()),
						callDAG.ComponentOf(succ.Node().Function()),
					)
				}

				exitNode := node.Derive(exit)
				reachableExits[exitNode] = true
				return chargedReturns.Edges(exitNode)
			}).BFSV(func(node defs.CtrLoc) bool {
				if node.Node().Function() == cl.Root() {
					reachesRoot = true
				}
				return false
			}, succ, succ.WithExiting(true))

			if !reachesRoot { // safety check
				log.Fatalf("Mistakes were made when returning from %s to %s %v", cl, succ, reachableExits)
			}

			// Remove all charged return edges from function exit nodes we did not reach
			newChargedReturns := chargedReturns
			chargedReturns.ForEach(func(cl defs.CtrLoc, _ L.InfSet[defs.CtrLoc]) {
				if _, isExit := cl.Node().(*cfg.FunctionExit); isExit && !reachableExits[cl] {
					newChargedReturns = newChargedReturns.Remove(cl)
				}
			})

			updatedRetState = retState.UpdateThreadCharges(
				retState.ThreadCharges().Update(g, newChargedReturns),
			)
		}

		// If the call instruction is a normal call (not defer), we need
		// to propagate the return value.
		if ssaNode, ok := succ.Node().CallRelationNode().(*cfg.SSANode); ok {
			if !hasReturnVal {
				panic(fmt.Errorf("missing return value when returning from %v", n))
			}

			value := ssaNode.Instruction().(*ssa.Call)
			updatedRetState = updatedRetState.UpdateMemory(
				updatedRetState.Memory().Update(loc.LocationFromSSAValue(g, value), returnVal),
			)
		}

		succs = succs.WeakUpdate(succ, updatedRetState)
	}

	return succs
}
