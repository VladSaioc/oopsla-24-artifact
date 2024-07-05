// git_link=loc-530-ginger.pml
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
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  int y14 = x2;
  int y15 = x1;
  chan c16 = [y15] of {int};
  run wg_monitor(w13);
  for(y9 : 0 .. (y14) - (1)) {
    for10: skip;
    w13.update!1;
    w13.update_ack?y8;
    assert y8;
    run fun4(w13, c16, child_12);
    run receiver(child_12);
    c16!0;
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  stop_process: skip;
  child_6!0;
}
proctype fun4(Wgdef w17; chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  defer1: skip;
  skip;
  c18?0;
  w17.update!-(1);
  w17.update_ack?y21;
  assert y21;
  stop_process: skip;
  child_19!0;
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