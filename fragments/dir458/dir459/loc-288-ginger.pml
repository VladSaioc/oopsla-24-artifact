// git_link=loc-288-ginger.pml
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
  chan child_8 = [1] of {int};
  run fun3(child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun3(chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  Wgdef w18 ;
  int y19 = x2;
  int y20 = x1;
  run wg_monitor(w18);
  chan c21 = [0] of {int};
  chan c22 = [y20] of {int};
  w18.update!1;
  w18.update_ack?y11;
  assert y11;
  run fun4(c21, c22, w18, y20, child_17);
  run receiver(child_17);
  for(y12 : 0 .. (y19) - (1)) {
    for20: skip;
    w18.update!1;
    w18.update_ack?y11;
    assert y11;
    run fun5(c21, c22, w18, child_16);
    run receiver(child_16);
    for20_end: skip;
  };
  for20_exit: skip;
  w18.update!1;
  w18.update_ack?y11;
  assert y11;
  run fun7(c21, c22, w18, y20, child_15);
  run receiver(child_15);
  w18.wait?0;
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c23; chan c24; Wgdef w25; int y26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  if
  :: ((0) != (-(2))) && (((y26) - (1)) != (-(3))) ->
    for(y30 : 0 .. (y26) - (1)) {
      for11: skip;
      c23!0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      c23!0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
}
proctype fun5(chan c34; chan c35; Wgdef w36; chan child_37) {
  bool y38 = false;
  bool y39 = false;
  int y40 = 0;
  bool y41 = true;
  int y42 = 0;
  for21_exit: skip;
  defer1: skip;
  skip;
  w36.update!-(1);
  w36.update_ack?y39;
  assert y39;
  stop_process: skip;
  child_37!0;
}
proctype fun6(chan c43; chan c44; Wgdef w45; chan child_46) {
  bool y47 = false;
  bool y48 = false;
  int y49 = 0;
  bool y50 = true;
  int y51 = 0;
  c44!0;
  defer1: skip;
  skip;
  w45.update!-(1);
  w45.update_ack?y48;
  assert y48;
  stop_process: skip;
  child_46!0;
}
proctype fun7(chan c52; chan c53; Wgdef w54; int y55; chan child_56) {
  bool y57 = false;
  bool y58 = false;
  int y59 = 0;
  bool y60 = true;
  int y61 = 0;
  if
  :: ((0) != (-(2))) && (((y55) - (1)) != (-(3))) ->
    for(y59 : 0 .. (y55) - (1)) {
      for31: skip;
      c53?0;
      for31_end: skip;
    };
    for31_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c53?0;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  defer1: skip;
  skip;
  w54.update!-(1);
  w54.update_ack?y58;
  assert y58;
  stop_process: skip;
  child_56!0;
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