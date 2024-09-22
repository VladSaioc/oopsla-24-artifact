package config

import "time"

// Dangerous thresholds: these thresholds prevents the checker or part of the checker from timeout
//
//	if these thresholds are reached, must print "!!!!" and a warning to the terminal
var MAX_GCATCH_DDL_SECOND = 360000 // 100 h
const MAX_LCA_LAYER = 7            // The maximum caller-callee layers when updating dependency map and finding LCA (Lowest Common Ancester)
var MAX_GCATCH_FRAGMENT_ANALYSIS_TIME = 0

// Points-to analysis timeout duration. 0 means no timeout.
var MAX_PTA_DURATION time.Duration = 0

const MAX_INST_IN_SYNCGRAPH = 10000
const Max_PATH_ENUMERATE int = 1000000000
const MAX_PATH_ENUMERATE_SECOND = 60

// End of Dangerous thresholds

const Print_Debug_Info = false
const DISABLE_OPTIMIZATION_CALLEES = false // If set to false, we won't enter every callee while building syncgraph
const POINTER_CONSIDER_REFLECTION = false

// flag constants
const ConstPrintDeferMap = "print-defer-map"

// Parameters used to tune only the checkers/structfield checker
const STRUCT_MIN_TIME_OF_USAGE = 4
const STRUCT_RATIO float32 = 0.75
const STRUCT_FP_RATIO float32 = 0.5
const STRUCT_FP_LAYER int = 3
