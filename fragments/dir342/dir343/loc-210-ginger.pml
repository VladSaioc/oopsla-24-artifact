// git_link=loc-210-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  Wgdef w12 ;
  int y13 = x1;
  run wg_monitor(w12);
  w12.update!y13;
  w12.update_ack?y7;
  assert y7;
  chan c14 = [1] of {int};
  for(y8 : 0 .. (y13) - (1)) {
    for10: skip;
    run fun3(c14, w12, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: true ->
    run close(c14);

  fi;
  for(y10 : 0 .. (y13) - (1)) {
    for20: skip;
    c14?0;
    for20_end: skip;
  };
  for20_exit: skip;
  w12.wait?0;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c16; Wgdef w17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  stop_process: skip;
  c16!0;
  w17.update!-1;
  w17.update_ack?y20;
  child_18!0;
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
