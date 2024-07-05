// git_link=loc-577-ginger.pml
#define  default true
#define  x1 ??
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
  int y11 = x1;
  Wgdef w12 ;
  run wg_monitor(w12);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  for(y7 : 0 .. (y11) - (1)) {
    for10: skip;
    if
    :: true ->
      w12.update!1;
      w12.update_ack?y6;
      assert y6;
      run fun2(w12, child_10);
      run receiver(child_10);

    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  w12.wait?0;
  goto stop_process;
  stop_process: skip;
  child_4!0;
}
proctype fun2(Wgdef w15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
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
