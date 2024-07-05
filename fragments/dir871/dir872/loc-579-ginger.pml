// git_link=loc-579-ginger.pml
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
  run wg_monitor(w15);
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    if
    :: true ->
      if
      :: true ->
        w15.update!1;
        w15.update_ack?y9;
        assert y9;
        run fun3(w15, child_14);
        run receiver(child_14);
      :: true ->


      fi;
      w15.update!1;
      w15.update_ack?y9;
      assert y9;
      run fun4(w15, child_13);
      run receiver(child_13);

    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(Wgdef w18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  defer1: skip;
  skip;
  w18.update!-(1);
  w18.update_ack?y21;
  assert y21;
  stop_process: skip;
  child_19!0;
}
proctype fun4(Wgdef w25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y28;
  assert y28;
  stop_process: skip;
  child_26!0;
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