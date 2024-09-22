/*
 * Project: kubernetes
 * Issue or PR  : https://github.com/kubernetes/kubernetes/pull/62464
 * Buggy version: a048ca888ad27367b1a7b7377c67658920adbf5d
 * fix commit-id: c1b19fce903675b82e9fdd1befcc5f5d658bfe78
 * Flaky: 8/100
 * Description:
 *   This is another example for recursive read lock bug. It has
 * been noticed by the go developers that RLock should not be
 * recursively used in the same thread.
 */

package main

import (
	"math/rand"
)

type State interface {
	GetCPUSetOrDefault()
	GetCPUSet() bool
	GetDefaultCPUSet()
	SetDefaultCPUSet()
}

type stateMemory struct {
	w chan bool
	r chan bool
}

func (s *stateMemory) GetCPUSetOrDefault() {
	s.r <- true
	defer func() { <-s.r }()
	if ok := s.GetCPUSet(); ok {
		return
	}
	s.GetDefaultCPUSet()
}

func (s *stateMemory) GetCPUSet() bool {
	s.r <- true
	defer func() { <-s.r }()

	if rand.Intn(10) > 5 {
		return true
	}
	return false
}

func (s *stateMemory) GetDefaultCPUSet() {
	s.r <- true
	defer func() { <-s.r }()
}

func (s *stateMemory) SetDefaultCPUSet() {
	s.w <- true
	defer func() { <-s.w }()
}

type staticPolicy struct{}

func (p *staticPolicy) RemoveContainer(s State) {
	s.GetDefaultCPUSet()
	s.SetDefaultCPUSet()
}

type manager struct {
	state *stateMemory
}

func (m *manager) reconcileState() {
	m.state.GetCPUSetOrDefault()
}

func NewPolicyAndManager() (*staticPolicy, *manager) {
	_s := func() (lock stateMemory) {
		lock = stateMemory{
			w: make(chan bool),
			r: make(chan bool),
		}

		go func() {
			rCount := 0

			// As long as all locks are free, both a reader
			// and a writer may acquire the lock
			for {
				select {
				// If a writer acquires the lock, hold it until released
				case <-lock.w:
					lock.w <- false
					// If a reader acquires the lock, step into read-mode.
				case <-lock.r:
					// Increment the reader count
					rCount++
					// As long as not all readers are released, stay in read-mode.
					for rCount > 0 {
						select {
						// One reader released the lock
						case lock.r <- false:
							rCount--
							// One reader acquired the lock
						case <-lock.r:
							rCount++
						}
					}
				}
			}
		}()

		return lock
	}()
	s := &_s
	m := &manager{s}
	p := &staticPolicy{}
	return p, m
}

///
/// G1 									G2
/// m.reconcileState()
/// m.state.GetCPUSetOrDefault()
/// s.RLock()
/// s.GetCPUSet()
/// 									p.RemoveContainer()
/// 									s.GetDefaultCPUSet()
/// 									s.SetDefaultCPUSet()
/// 									s.Lock()
/// s.RLock()
/// ---------------------G1,G2 deadlock---------------------
///
func main() {
	p, m := NewPolicyAndManager()
	go m.reconcileState()
	go p.RemoveContainer(m.state)
}
