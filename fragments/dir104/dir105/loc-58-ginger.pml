// git_link=loc-58-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun2(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun2(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  int y17 = -(2);
  int y18 = -(2);
  int y19 = -(2);
  int y20 = -(2);
  int y21 = -(2);
  int y22 = x1;
  int y23 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c25 = [0] of {int};
  chan c26 = [y22] of {int};
  chan c27 = [y22] of {int};
  run fun3(c25, child_16);
  run receiver(child_16);
  run fun4(c26, c27, y22, child_15);
  child_15?0;
  c25?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: ((0) != (-(2))) && (((y22) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y22) - (1)) {
      for41: skip;
      c26?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      c27?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for41_end: skip;
    };
    for41_exit: skip;
  :: else  ->
    do
    :: true ->
      for40: skip;
      c26?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      c27?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for40_end: skip;
    :: true ->
      break;

    od;
    for40_exit: skip;
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
proctype fun3(chan c33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  int y40 = -(2);
  c33!0;
  stop_process: skip;
  child_34!0;
}
proctype fun4(chan c41; chan c42; int y43; chan child_44) {
  bool y45 = false;
  bool y46 = false;
  int y47 = 0;
  bool y48 = true;
  int y49 = 0;
  chan child_50 = [1] of {int};
  Wgdef w51 ;
  run wg_monitor(w51);
  for(y47 : 0 .. (y43) - (1)) {
    for30: skip;
    w51.update!1;
    w51.update_ack?y46;
    assert y46;
    run fun5(c42, c41, w51, child_50);
    run receiver(child_50);
    for30_end: skip;
  };
  for30_exit: skip;
  w51.wait?0;
  stop_process: skip;
  child_44!0;
}
proctype fun5(chan c52; chan c53; Wgdef w54; chan child_55) {
  bool y56 = false;
  bool y57 = false;
  int y58 = 0;
  bool y59 = true;
  int y60 = 0;
  chan child_61 = [1] of {int};
  run fun6(c53, c52, child_61);
  child_61?0;
  defer1: skip;
  skip;
  w54.update!-(1);
  w54.update_ack?y57;
  assert y57;
  stop_process: skip;
  child_55!0;
}
proctype fun6(chan c62; chan c63; chan child_64) {
  bool y65 = false;
  bool y66 = false;
  int y67 = 0;
  bool y68 = true;
  int y69 = 0;
  c62!0;
  c63!0;
  stop_process: skip;
  child_64!0;
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