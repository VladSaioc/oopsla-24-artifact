// git_link=loc-501-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_5 = [1] of {int};
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w14);
  w14.update!y15;
  w14.update_ack?y8;
  assert y8;
  for(y9 : 0 .. (y15) - (1)) {
    for10: skip;
    run fun3(w14, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  w14.update!1;
  w14.update_ack?y8;
  assert y8;
  run fun4(w14, child_12);
  run receiver(child_12);
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Wgdef w17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  w17.update!-(1);
  w17.update_ack?y20;
  assert y20;
  stop_process: skip;
  child_18!0;
}
proctype fun4(Wgdef w24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  w24.update!-(1);
  w24.update_ack?y27;
  assert y27;
  stop_process: skip;
  child_25!0;
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