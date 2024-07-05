// git_link=loc-586-ginger.pml
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
  int y17 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c19 = [0] of {int};
  chan c20 = [0] of {int};
  chan c21 = [0] of {int};
  chan c22 = [0] of {int};
  run wg_monitor(w16);
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    w16.update!1;
    w16.update_ack?y10;
    assert y10;
    run fun4(c19, c20, c21, c22, w16, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun5(c22, c19, c20, c21, w16, child_14);
  run receiver(child_14);
  c19?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c25; chan c26; chan c27; chan c28; Wgdef w29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  if
  :: true ->
    c28!0;
  :: true ->
    if
    :: true ->
      c26!0;
    :: true ->
      c27!0;

    fi;

  fi;
  w29.update!-(1);
  w29.update_ack?y32;
  assert y32;
  stop_process: skip;
  child_30!0;
}
proctype fun5(chan c38; chan c39; chan c40; chan c41; Wgdef w42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  w42.wait?0;
  if
  :: true ->
    c39!0;
  :: true ->


  fi;
  run close(c39);
  run close(c38);
  run close(c40);
  run close(c41);
  stop_process: skip;
  child_43!0;
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