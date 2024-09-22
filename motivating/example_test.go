package motivating

import "testing"

func TestGetResultsA(t *testing.T) {
	for i := -1; i < 10; i++ {
		func() {
			defer recover()
			GetResultsA(i)
		}()
	}
}

func TestGetResultsB(t *testing.T) {
	for i := -1; i < 10; i++ {
		func() {
			defer recover()
			GetResultsB(i)
		}()
	}
}

func TestGetResultsC(t *testing.T) {
	for i := -1; i < 10; i++ {
		func() {
			defer recover()
			GetResultsC(i)
		}()
	}
}

func TestDivisionExample(t *testing.T) {
	for i := -3; i < 10; i++ {
		func() {
			defer recover()
			DivisionExample(i)
		}()
	}
}

func TestUnverifiableExample(t *testing.T) {
	for i := -1; i < 10; i++ {
		func() {
			defer recover()
			UnverifiableExample(i, i)
		}()
	}
}
