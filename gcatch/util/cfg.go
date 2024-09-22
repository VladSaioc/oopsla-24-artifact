package util

import (
	"fmt"

	"golang.org/x/tools/go/ssa"
)

func GetFirstInst(bb *ssa.BasicBlock) ssa.Instruction {
	if len(bb.Instrs) == 0 {
		return nil
	}
	return bb.Instrs[0]
}

func GetLastInst(bb *ssa.BasicBlock) ssa.Instruction {

	if len(bb.Instrs) == 0 {
		return nil
	}

	return bb.Instrs[len(bb.Instrs)-1]
}

func IsFnBegin(ii ssa.Instruction) bool {
	bb := *ii.Parent().Blocks[0]
	return bb.Instrs[0] == ii
}

func IsFnEnd(ii ssa.Instruction) bool {
	fn := ii.Parent()
	if len(fn.Blocks) == 0 {
		return false
	}

	for _, bb := range fn.Blocks {
		if len(bb.Succs) == 0 {
			if ii == GetLastInst(bb) {
				return true
			}
		}
	}

	return false
}

// according to the annotation of ssa.Function, a function only has one normal entry BB and one optional recover BB.
// There is no need to analyze the recover BB. No BB can reach it and it reaches no BB
func GetEntryInsts(fn *ssa.Function) []ssa.Instruction {

	vecResult := make([]ssa.Instruction, 0)

	for _, bb := range fn.Blocks {
		if bb.Comment == "recover" {
			continue
		}
		if len(bb.Preds) == 0 {
			if len(bb.Instrs) > 0 {
				vecResult = append(vecResult, bb.Instrs[0])
				break
			}
		}
	}

	return vecResult
}

func GetExitInsts(fn *ssa.Function) []ssa.Instruction {
	vecResult := make([]ssa.Instruction, 0)
	for _, bb := range fn.Blocks {
		if len(bb.Succs) == 0 {
			if len(bb.Instrs) > 0 {
				vecResult = append(vecResult, bb.Instrs[len(bb.Instrs)-1])
			}
		}
	}

	return vecResult
}

func GetPrevInsts(inputInst ssa.Instruction) []ssa.Instruction {

	vecResult := make([]ssa.Instruction, 0)

	if IsFnBegin(inputInst) {
		return vecResult
	}

	for _, bb := range inputInst.Parent().Blocks {
		if inputInst == bb.Instrs[0] {
			for _, pred := range bb.Preds {
				vecResult = append(vecResult, GetLastInst(pred))
			}

			return vecResult
		}
	}

	for _, bb := range inputInst.Parent().Blocks {
		for index, _ := range bb.Instrs {
			if inputInst == bb.Instrs[index] {
				vecResult = append(vecResult, bb.Instrs[index-1])
				return vecResult
			}
		}
	}

	fmt.Println("Error when calculating previous insts for inst:", inputInst)
	panic(inputInst)

}

func GetSuccInsts(inputInst ssa.Instruction) []ssa.Instruction {

	vecResult := make([]ssa.Instruction, 0)

	if IsFnEnd(inputInst) {
		return vecResult
	}

	for _, bb := range inputInst.Parent().Blocks {
		if inputInst == GetLastInst(bb) {
			for _, succ := range bb.Succs {
				vecResult = append(vecResult, GetFirstInst(succ))
			}

			return vecResult
		}
	}

	for _, bb := range inputInst.Parent().Blocks {
		for index, _ := range bb.Instrs {
			if inputInst == bb.Instrs[index] {
				vecResult = append(vecResult, bb.Instrs[index+1])

				return vecResult
			}
		}
	}

	fmt.Println("Error when calculating previous insts for inst:", inputInst)
	panic(inputInst)
}

func GetIIndexBB(II ssa.Instruction) int {
	index := 0
	for index < len(II.Block().Instrs) {
		if II.Block().Instrs[index] == II {
			return index
		}

		index = index + 1
	}

	return -1
}
