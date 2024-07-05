// git_link=loc-333-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
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
  run fun3(x1, x2, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  int y16 = y7;
  if
  :: (y16) == (0) ->
    goto stop_process;
  :: else  ->
  fi;
  run wg_monitor(w15);
  chan c18 = [y6] of {int};
  w15.update!y16;
  w15.update_ack?y10;
  assert y10;
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    c18!0;
    run fun4(c18, w15, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c19; Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  defer1: skip;
  skip;
  c19?0;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
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