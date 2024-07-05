// git_link=loc-440-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  Wgdef w16 ;
  int y17 = x3;
  int y18 = x2;
  int y19 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w16);
  for(y10 : 0 .. (y19) - (1)) {
    for10: skip;
    w16.update!1;
    w16.update_ack?y9;
    assert y9;
    run fun5(w16, child_15);
    run receiver(child_15);
    for(y10 : 0 .. (y18) - (1)) {
      for11: skip;
      w16.update!1;
      w16.update_ack?y9;
      assert y9;
      run fun5(w16, child_14);
      run receiver(child_14);
      for11_end: skip;
    };
    for11_exit: skip;
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: true ->
    for(y10 : 0 .. (y17) - (1)) {
      for20: skip;
      w16.update!1;
      w16.update_ack?y9;
      assert y9;
      run fun5(w16, child_13);
      run receiver(child_13);
      for20_end: skip;
    };
    for20_exit: skip;
  :: true ->


  fi;
  w16.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(Wgdef w22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  defer1: skip;
  skip;
  w22.update!-(1);
  w22.update_ack?y25;
  assert y25;
  stop_process: skip;
  child_23!0;
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