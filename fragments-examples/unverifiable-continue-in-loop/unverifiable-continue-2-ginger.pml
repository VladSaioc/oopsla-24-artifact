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
  run wg_monitor(w12);
  for(y8 : 0 .. (y13) - (1)) {
    for10: skip;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    w12.update!1;
    w12.update_ack?y7;
    assert y7;
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
proctype fun3(Wgdef w16; chan child_17) {
  bool y18 = false;
  bool y19 = false;
  int y20 = 0;
  bool y21 = true;
  int y22 = 0;
  if
  :: true ->
    w16.update!-(1);
    w16.update_ack?y19;
    assert y19;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    w16.update!-(1);
    w16.update_ack?y19;
    assert y19;
    goto stop_process;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w16.update!-(1);
  w16.update_ack?y19;
  assert y19;
  stop_process: skip;
  child_17!0;
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
