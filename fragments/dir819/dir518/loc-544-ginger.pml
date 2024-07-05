// git_link=loc-544-ginger.pml
#define  default true
typedef Chandef {
  chan sync = [0] of {bool};
  chan enq = [0] of {bool};
  chan deq = [0] of {bool,bool};
  chan sending = [0] of {bool};
  chan rcving = [0] of {bool};
  chan closing = [0] of {bool};
  int size = 0;
  int num_msgs = 0;
  bool closed = false;
}
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_3 = [1] of {int};
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  Wgdef w11 ;
  int y12 = 3;
  chan c13 = [0] of {int};
  run wg_monitor(w11);
  for(y7 : 0 .. (y12) - (1)) {
    for10: skip;
    w11.update!1;
    w11.update_ack?y6;
    assert y6;
    run fun2(c13, w11, child_10);
    run receiver(child_10);
    for10_end: skip;
  };
  for10_exit: skip;
  w11.wait?0;
  run close(c13);
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c14; Wgdef w15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  w15.update!-(1);
  w15.update_ack?y18;
  assert y18;
  c14?0;
  stop_process: skip;
  child_16!0;
}
proctype wg_monitor(Wgdef wg) {
  int i = 0;
  end: skip;
  do
  :: wg.update?i ->
    wg.Counter = (wg.Counter) + (i);
    wg.update_ack!(wg.Counter) >= (0);
  :: (wg.Counter) == (0) ->
    end1: skip;
    if
    :: wg.update?i ->
      wg.Counter = (wg.Counter) + (i);
      wg.update_ack!(wg.Counter) >= (0);
    :: wg.wait!0 ->


    fi;

  od;
}
proctype receiver(chan c) {
  c?0;
}