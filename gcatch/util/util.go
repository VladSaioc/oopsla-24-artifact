package util

import (
	"github.com/system-pclub/GCatch/GCatch/config"
	"golang.org/x/exp/slices"
	"golang.org/x/tools/go/ssa"

	"fmt"
	"runtime"
	"sync"
)

func IsInstInVec(inst ssa.Instruction, vec []ssa.Instruction) bool {
	for _, elem := range vec {
		if elem == inst {
			return true
		}
	}
	return false
}

func VecFnForVecInst(vecInst []ssa.Instruction) []*ssa.Function {
	result := []*ssa.Function{}

	mapFn := make(map[*ssa.Function]struct{})
	for _, inst := range vecInst {
		mapFn[inst.Parent()] = struct{}{}
	}

	for fn, _ := range mapFn {
		result = append(result, fn)
	}

	return result
}

// PathIncluded checks for a given element whether its stringified form is included in a
// a set of accepted paths.
func PathIncluded(fn *ssa.Function) bool {
	return fn != nil && config.IsPathIncluded(fn.String())
}

// ParallelIntraproceduralAnalysis
func ParallelIntraproceduralAnalysis(Checker string, fns map[*ssa.Function]bool, f func(fn *ssa.Function)) {
	par, wg := make(chan struct{}, runtime.NumCPU()), &sync.WaitGroup{}
	okFns := make([]*ssa.Function, 0, len(fns))
	for fn := range fns {
		if PathIncluded(fn) {
			okFns = append(okFns, fn)
		}
	}
	slices.SortFunc(okFns, func(f1, f2 *ssa.Function) bool {
		return f1.Pos() < f2.Pos()
	})

	counter := 0
	wg.Add(len(okFns))
	for _, fn := range okFns {
		go func(fn *ssa.Function) {
			defer wg.Done()
			defer func() {
				<-par
			}()
			par <- struct{}{}

			config.BugIndexMu.Lock()
			fmt.Printf("%s :: Checking fragment [%d/%d]\n", Checker, counter, len(okFns)-1)
			counter++
			config.BugIndexMu.Unlock()

			f(fn)
		}(fn)
	}
	wg.Wait()
}

// IntraproceduralAnalysis
func IntraproceduralAnalysis(Checker string, fns map[*ssa.Function]bool, f func(fn *ssa.Function)) {
	okFns := make([]*ssa.Function, 0, len(fns))
	for fn := range fns {
		if PathIncluded(fn) {
			okFns = append(okFns, fn)
		}
	}
	slices.SortFunc(okFns, func(f1, f2 *ssa.Function) bool {
		return f1.Pos() < f2.Pos()
	})

	counter := 0
	for _, fn := range okFns {
		fmt.Printf("%s :: Checking fragment [%d/%d]\n", Checker, counter, len(okFns)-1)
		counter++
		f(fn)
	}
}
