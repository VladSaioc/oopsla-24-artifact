// git_link=loc-263-ginger.pml
#define  default true
#define  x1 ??
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
  chan child_5 = [1] of {int};
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  chan c16 = [0] of {int};
  chan c17 = [0] of {int};
  run wg_monitor(w15);
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    w15.update!1;
    w15.update_ack?y9;
    assert y9;
    run fun3(c17, c16, w15, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun4(c16, c17, w15, child_13);
  run receiver(child_13);
  defer1: skip;
  skip;
  w15.wait?0;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c18; chan c19; Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  defer1: skip;
  skip;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun4(chan c27; chan c28; Wgdef w29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  int y36 = -(2);
  run close(c27);
  w29.wait?0;
  run close(c28);
  stop_process: skip;
  child_30!0;
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