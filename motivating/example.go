package motivating

import "sync"

func GetResultsA(x int) {
	c := make(chan struct{})
	for i := 0; i < x; i++ {
		go func() {
			c <- struct{}{}
		}()
	}
	<-c
}

func GetResultsB(x int) {
	c := make(chan struct{}, x)
	for i := 0; i < x; i++ {
		go func() {
			c <- struct{}{}
		}()
	}
	<-c
}

func GetResultsC(x int) {
	if x <= 0 {
		return
	}
	c := make(chan struct{}, x)
	for i := 0; i < x; i++ {
		go func() {
			c <- struct{}{}
		}()
	}
	<-c
}

func UnverifiableExample(complianceLevels, numLevels int) {
	if numLevels == 0 {
		return
	}
	wg := &sync.WaitGroup{}
	c := make(chan struct{}, numLevels)
	wg.Add(complianceLevels)
	for i := 0; i < complianceLevels; i++ {
		go func() {
			c <- struct{}{}
			wg.Done()
		}()
	}
	wg.Wait()
	for j := 0; j < numLevels; j++ {
		<-c
	}
}

func DivisionExample(readers int) {
	done := make(chan struct{})
	go func() {
		done <- struct{}{}
	}()
	for i := 0; i < readers/2; i++ {
		go func() {
			done <- struct{}{}
		}()
	}
	go func() {
		done <- struct{}{}
	}()
	for j := readers / 2; j < readers; j++ {
		go func() {
			done <- struct{}{}
		}()
	}
	for i := 0; i < readers+2; i++ {
		<-done
	}
}
