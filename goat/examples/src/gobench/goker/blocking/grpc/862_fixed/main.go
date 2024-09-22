/*
 * Project: grpc-go
 * Issue or PR   : https://github.com/grpc/grpc-go/pull/862
 * Buggy version: d8f4ebe77f6b7b6403d7f98626de8a534f9b93a7
 * fix commit-id: dd5645bebff44f6b88780bb949022a09eadd7dae
 * Flaky: 100/100
 * Description:
 *   When return value conn is nil, cc (ClientConn) is not closed.
 * The goroutine executing resetAddrConn is leaked. The patch is to
 * close ClientConn in the defer func().
 */
package main

import (
	"context"
	"time"
)

type ClientConn struct {
	ctx    context.Context
	cancel context.CancelFunc
	conns  []*addrConn
}

func (cc *ClientConn) Close() {
	cc.cancel()
	conns := cc.conns
	cc.conns = nil
	for _, ac := range conns {
		ac.tearDown()
	}
}

func (cc *ClientConn) resetAddrConn() {
	ac := &addrConn{
		cc: cc,
	}
	cc.conns = append(cc.conns, ac)
	ac.ctx, ac.cancel = context.WithCancel(cc.ctx)
	ac.resetTransport()
}

type addrConn struct {
	cc     *ClientConn
	ctx    context.Context
	cancel context.CancelFunc
}

func (ac *addrConn) resetTransport() {
	for retries := 1; ; retries++ {
		_ = 2 * time.Nanosecond * time.Duration(retries)
		timeout := 10 * time.Nanosecond
		_, cancel := context.WithTimeout(ac.ctx, timeout)
		_ = time.Now()
		cancel()
		<-ac.ctx.Done() //@ releases(g2)
		return
		/*
		select { // Block here // GoLive: this cannot block forever?
		case <-time.After(sleepTime - time.Since(connectTime)):
		case <-ac.ctx.Done():
			return
		}
		*/
	}
}

func (ac *addrConn) tearDown() {
	ac.cancel()
}

func DialContext(ctx context.Context) (conn *ClientConn) {
	cc := &ClientConn{}
	cc.ctx, cc.cancel = context.WithCancel(context.Background())
	defer func() {
		select {
		case <-ctx.Done():
			if conn != nil {
				conn.Close()
			}
			conn = nil
		default:
		}
		cc.Close() /// FIX: cc.Close()
	}()
	go func() { // G2 //@ go(go2)
		cc.resetAddrConn()
	}()
	return conn
}

///
/// G1 					G2
/// DialContext()
/// 					cc.resetAddrConn()
/// 					resetTransport()
/// 					<-ac.ctx.Done()
/// --------------G2 leak------------------
///

//@ goro(g2, true, _root, go1, go2)

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	go DialContext(ctx) // G1 //@ go(go1)
	go cancel()         // helper goroutine
}
