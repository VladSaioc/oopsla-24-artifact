package fatal

import (
	"fmt"
	"strings"
	"sync"

	"github.com/system-pclub/GCatch/GCatch/config"
	"github.com/system-pclub/GCatch/GCatch/output"
	"github.com/system-pclub/GCatch/GCatch/util"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"
)

var C8_done_fn = make(map[string]struct{})

func Detect() {
	loop_fns()
}

func report(inst ssa.Instruction, parent *ssa.Function) {
	config.BugIndexMu.Lock()
	config.BugIndex++
	fmt.Print("----------Bug[")
	fmt.Print(config.BugIndex)
	config.BugIndexMu.Unlock()
	fmt.Print("]----------\n\tType: API-Fatal \tReason: testing.Fatal()/FailNow()/SkipNow()/... can only be used in test goroutine.\n")
	fmt.Print("\tLocation of call:\n")
	output.PrintIISrc(inst)
}

func loop_fns() {
	mu := &sync.Mutex{}
	util.ParallelIntraproceduralAnalysis("Fatal", ssautil.AllFunctions(config.Prog), func(fn *ssa.Function) {
		mu.Lock()
		if _, ok := C8_done_fn[fn.String()]; ok {
			mu.Unlock()
			return
		}
		C8_done_fn[fn.String()] = struct{}{}
		mu.Unlock()

		if _, timer, ok := util.ExecuteWithinTimeFrame(fn, inside_func); ok {
			fmt.Println("Fatal :: Fragment analysis took:", timer)
		} else {
			fmt.Printf("Fatal :: Fragment analysis timed out in %ds\n", config.MAX_GCATCH_FRAGMENT_ANALYSIS_TIME)
		}
	})
}

func inside_func(fn *ssa.Function) bool {
	var bugFound bool

	for _, bb := range fn.Blocks {
		for _, inst := range bb.Instrs {
			//p := (config.Prog.Fset).Position(inst.Pos())

			inst_go, ok := inst.(*ssa.Go)
			if !ok {
				continue
			}

			if inst_go.Call.IsInvoke() == true {
				continue
			}

			callee := inst_go.Call.Value
			var interesting_fn *ssa.Function
			switch concrete := callee.(type) {
			case *ssa.Function:
				interesting_fn = concrete
			case *ssa.Builtin:
			case *ssa.MakeClosure:
				var ok bool
				interesting_fn, ok = concrete.Fn.(*ssa.Function)
				if !ok {
					fmt.Println("Warning in C8: Unknown MakeClosure callee in:", fn.String(), "\tinst:", inst)
				}
			default:
				node, ok := config.CallGraph.Nodes[fn]
				if !ok {
					continue
				}
				for _, out := range node.Out {
					if out.Site == inst {
						if out.Callee.Func != nil {
							if strings.Contains(out.Callee.Func.String(), fn.Name()) { // make sure the callee is created in this function, or there will be a lot of FPs
								bugFound = bugFound || find_fatal_in_fn(out.Callee.Func, fn)
							}
						}
					}
				}

			}
			if interesting_fn == nil {
				continue
			} else {
				bugFound = bugFound || find_fatal_in_fn(interesting_fn, fn)
			}
		}
	}

	return bugFound
}

func find_fatal_in_fn(target, parent *ssa.Function) bool {
	var bugFound bool

	for _, bb := range target.Blocks {
		for _, inst := range bb.Instrs {
			inst_call, ok := inst.(*ssa.Call)
			if !ok {
				continue
			}

			if inst_call.Call.IsInvoke() {
				continue
			}

			callee_fn, ok := inst_call.Call.Value.(*ssa.Function)
			if !ok {
				continue
			}

			if callee_fn.Name() == "Fatal" || callee_fn.Name() == "Fatalf" || callee_fn.Name() == "FailNow" ||
				callee_fn.Name() == "Skip" || callee_fn.Name() == "Skipf" || callee_fn.Name() == "SkipNow" {
				if strings.Contains(callee_fn.Pkg.String(), "package testing") == false {
					continue
				}
				bugFound = true
				report(inst, parent)
			}
		}
	}
	for _, anony := range target.AnonFuncs {
		find_fatal_in_fn(anony, parent)
	}

	return bugFound
}
