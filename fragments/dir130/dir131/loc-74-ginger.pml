// git_link=loc-74-ginger.pml
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
  Wgdef w13 ;
  int y14 = x1;
  run wg_monitor(w13);
  for(y9 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun3(w13, child_12);
    child_12?0;
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Wgdef w15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  chan child_22 = [1] of {int};
  w15.update!1;
  w15.update_ack?y18;
  assert y18;
  run fun4(w15, child_22);
  run receiver(child_22);
  stop_process: skip;
  child_16!0;
}
proctype fun4(Wgdef w23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  w23.update!-(1);
  w23.update_ack?y26;
  assert y26;
  stop_process: skip;
  child_24!0;
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