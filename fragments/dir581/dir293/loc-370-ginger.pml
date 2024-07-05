// git_link=loc-370-ginger.pml
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
  Wgdef w17 ;
  Wgdef w18 ;
  chan c19 = [100] of {int};
  chan c20 = [1] of {int};
  chan c21 = [0] of {int};
  run wg_monitor(w18);
  w18.update!1;
  w18.update_ack?y11;
  assert y11;
  run wg_monitor(w17);
  run fun4(c19, c20, c21, w18, w17, child_16);
  run receiver(child_16);
  for(y12 : 0 .. (30) - (1)) {
    for20: skip;
    run fun6(c19, w17, c20, c21, w18, child_15);
    run receiver(child_15);
    for20_end: skip;
  };
  for20_exit: skip;
  w18.wait?0;
  w17.wait?0;
  c20?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c22; chan c23; chan c24; Wgdef w25; Wgdef w26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  run fun5(c22, w26, child_33);
  child_33?0;
  c23!0;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
}
proctype fun5(chan c34; Wgdef w35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  int y42 = x1;
  for(y39 : 0 .. (x2) - (1)) {
    for10: skip;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      break;
    :: true ->


    fi;
    w35.update!y42;
    w35.update_ack?y38;
    assert y38;
    if
    :: ((y42) - (1)) != (-(3)) ->
      for(y39 : 0 .. (y42) - (1)) {
        for11: skip;
        c34!0;
        for11_end: skip;
      };
      for11_exit: skip;
    :: else  ->
      do
      :: true ->
        for12: skip;
        c34!0;
        for12_end: skip;
      :: true ->
        break;

      od;
      for12_exit: skip;
    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_36!0;
}
proctype fun6(chan c46; Wgdef w47; chan c48; chan c49; Wgdef w50; chan child_51) {
  bool y52 = false;
  bool y53 = false;
  int y54 = 0;
  bool y55 = true;
  int y56 = 0;
  for21_exit: skip;
  stop_process: skip;
  child_51!0;
}
proctype fun7(chan c57; chan c58; chan c59; Wgdef w60; Wgdef w61; chan child_62) {
  bool y63 = false;
  bool y64 = false;
  int y65 = 0;
  bool y66 = true;
  int y67 = 0;
  w60.update!-(1);
  w60.update_ack?y64;
  assert y64;
  stop_process: skip;
  child_62!0;
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