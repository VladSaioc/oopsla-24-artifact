// git_link=loc-48-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun3(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  Wgdef w16 ;
  run wg_monitor(w16);
  if
  :: true ->
    for(y11 : 0 .. (x2) - (1)) {
      for10: skip;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      w16.update!1;
      w16.update_ack?y10;
      assert y10;
      run fun4(w16, child_14);
      run receiver(child_14);
      for10_end: skip;
    };
    for10_exit: skip;
  :: true ->
    for(y11 : 0 .. (y7) - (1)) {
      for20: skip;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      w16.update!1;
      w16.update_ack?y10;
      assert y10;
      run fun5(w16, child_15);
      run receiver(child_15);
      for20_end: skip;
    };
    for20_exit: skip;

  fi;
  w16.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  defer1: skip;
  skip;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun5(Wgdef w27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  defer1: skip;
  skip;
  w27.update!-(1);
  w27.update_ack?y30;
  assert y30;
  stop_process: skip;
  child_28!0;
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