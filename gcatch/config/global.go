package config

import (
	"sync"

	"golang.org/x/tools/go/callgraph"
	"golang.org/x/tools/go/ssa"
)

//-path=/home/song/work/go-workspace/code/src/github.com/etcd-io/etcd -include=github.com/etcd-io/etcd

var StrEntrancePath string      // github.com/etcd-io/etcd
var StrGOPATH string            // /home/song/work/go-workspace/code
var StrAbsolutePath string      // /home/song/work/go-workspace/code/src/
var StrRelativePath string      // github.com/etcd-io/etcd
var MapPrintMod map[string]bool // a map indicates which information will be printed

// Variables for beta functionality that allows building and checking only one program using go.mod
var BoolGoMod bool
var StrModulePath string // Module path, like github.com/etcd-io/etcd
var StrModAbsPath string // Absolute path to the program. go.mod must be in this path

var BoolDisableFnPointer bool

var MapExcludePaths map[string]bool // a map indicates which package names should be ignored

var Prog *ssa.Program
var Pkgs []*ssa.Package

var BugIndex int
var BugIndexMu sync.Mutex

var VecPathStats []PathStat

var CallGraph *callgraph.Graph
var Inst2Defers map[ssa.Instruction][]*ssa.Defer
var Defer2Insts map[*ssa.Defer][]ssa.Instruction
var Inst2CallSite map[ssa.CallInstruction]map[*callgraph.Edge]bool

// Global variables used by BMOC checker
var MapHashOfCheckedCh map[string]struct{}
var BoolChSafety bool = false
