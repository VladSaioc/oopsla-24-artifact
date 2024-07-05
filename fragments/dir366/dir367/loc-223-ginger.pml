// git_link=loc-223-ginger.pml
#define  default true
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
  int y11 = 3;
  chan child_12 = [1] of {int};
  int y13 = 0;
  int y14 = 0;
  Wgdef w15 ;
  int y16 = 0;
  int y17 = -(2);
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;

  fi;
  run wg_monitor(w15);
  chan c21 = [(y14) + ((3) * (y13))] of {int};
  for(y7 : 0 .. (y14) - (1)) {
    for30: skip;
    w15.update!1;
    w15.update_ack?y6;
    assert y6;
    run fun2(c21, w15, child_12);
    run receiver(child_12);
    for30_end: skip;
  };
  for30_exit: skip;
  for(y7 : 0 .. (y13) - (1)) {
    for40: skip;
    for(y7 : 0 .. (y11) - (1)) {
      for41: skip;
      w15.update!1;
      w15.update_ack?y6;
      assert y6;
      run fun2(c21, w15, child_10);
      run receiver(child_10);
      for41_end: skip;
    };
    for41_exit: skip;
    for40_end: skip;
  };
  for40_exit: skip;
  w15.wait?0;
  if
  :: ((0) != (-(2))) && ((((y14) + (y13)) - (1)) != (-(3))) ->
    for(y7 : 0 .. ((y14) + (y13)) - (1)) {
      for51: skip;
      c21?0;
      for51_end: skip;
    };
    for51_exit: skip;
  :: else  ->
    do
    :: true ->
      for50: skip;
      c21?0;
      for50_end: skip;
    :: true ->
      break;

    od;
    for50_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c23; Wgdef w24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  c23!0;
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