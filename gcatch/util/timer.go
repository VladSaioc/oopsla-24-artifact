package util

import (
	"time"

	"github.com/system-pclub/GCatch/GCatch/config"
)

type Stopper chan struct{}

// NewStopper creates a channel that is stopped after a certain time period.
func NewStopper() Stopper {
	stop := make(Stopper)

	go func() {
		// If no timeout has been established, do not instrument an abort.
		if config.MAX_GCATCH_FRAGMENT_ANALYSIS_TIME == 0 {
			return
		}

		<-time.After(time.Duration(config.MAX_GCATCH_FRAGMENT_ANALYSIS_TIME) * time.Second)
		close(stop)
	}()

	return stop
}

// ExecuteWithinTimeFrame takes a value and a function, and attempts to execute the function on the given
// value. The function has to be executed within the time frame given by config.MAX_GCATCH_FRAGMENT_ANALYSIS_TIME.
// The values returned are the result of applying the function on the input, the time it took, and whether
// the timer expired before completion or not.
func ExecuteWithinTimeFrame[T, U any](x T, f func(T) U) (U, time.Duration, bool) {
	done, stop, timer := make(chan U, 1), NewStopper(), time.Now()

	go func() {
		done <- f(x)
	}()

	var y U
	var ok bool
	select {
	case y = <-done:
		ok = true
	case <-stop:
		ok = false
	}

	return y, time.Since(timer), ok
}

// IterateUntilTimeout takes an abort channel, a list of items, and a function that operates over individual items
// and their index. It iterates over every element of the list, and either executes the function or aborts
// if the stop channel has been closed. It returns true if stopped prematurely, or false if it successfully
// iterated over the entire list.
func IterateUntilTimeout[T any](stop Stopper, ts []T, f func(int, T) bool) bool {
	for i, t := range ts {
		select {
		case <-stop:
			return true
		default:
			if f(i, t) {
				return true
			}
		}
	}

	return false
}

// IterateUntilTimeout takes an abort channel, a list of items, and a function that operates over individual items
// and their index. It iterates over every element of the list, and either executes the function or aborts
// if the stop channel has been closed. It returns true if stopped prematurely, or false if it successfully
// iterated over the entire list.
func MapRangeUntilTimeout[T comparable, U any](stop Stopper, ts map[T]U, f func(T, U) bool) bool {
	for t, u := range ts {
		select {
		case <-stop:
			return true
		default:
			if f(t, u) {
				return true
			}
		}
	}

	return false
}

// LoopUntilTimeout takes a stopper and a function that is repeatedly executed until the guard returned
// is false, or aborts if when the stopper has been closed. It returns true if stopped prematurely, or false if
// all iterations succeed before the timer expires.
func (stop Stopper) LoopUntilTimeout(f func() bool) bool {
	for f() {
		select {
		case <-stop:
			return true
		default:
		}
	}

	return false
}
