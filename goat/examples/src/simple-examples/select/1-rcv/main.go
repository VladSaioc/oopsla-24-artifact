package main

func main() {
	ch := make(chan int)
	select {
	case <-ch: //@ analysis(false)
	}
}
