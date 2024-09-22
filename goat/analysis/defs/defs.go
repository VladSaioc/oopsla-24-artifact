package defs

import (
	u "github.com/cs-au-dk/goat/utils"

	c "github.com/fatih/color"
)

// colorize wraps colorization functions for consistent colorization of
// goroutines and superlocations.
var colorize = struct {
	Go       func(...interface{}) string
	Superloc func(...interface{}) string
	Panic    func(...interface{}) string
	Index    func(...interface{}) string
}{
	Go: func(is ...interface{}) string {
		return u.CanColorize(c.New(c.FgHiMagenta).SprintFunc())(is...)
	},
	Superloc: func(is ...interface{}) string {
		return u.CanColorize(c.New(c.FgHiBlue).SprintFunc())(is...)
	},
	Panic: func(is ...interface{}) string {
		return u.CanColorize(c.New(c.FgHiRed).SprintFunc())(is...)
	},
	Index: func(is ...interface{}) string {
		return u.CanColorize(c.New(c.FgHiCyan).SprintFunc())(is...)
	},
}
