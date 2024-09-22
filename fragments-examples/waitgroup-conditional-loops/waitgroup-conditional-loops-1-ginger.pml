#define  default true
#define  x1 ??
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w12);
  w12.update!y13;
  w12.update_ack?y7;
  assert y7;
  for(y8 : 0 .. (x1) - (1)) {
    for10: skip;
    run fun3(w12, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  w12.wait?0;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(Wgdef w15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  if
  :: true ->
    w15.update!-(1);
    w15.update_ack?y18;
    assert y18;
    goto stop_process;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w15.update!-(1);
  w15.update_ack?y18;
  assert y18;
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
