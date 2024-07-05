// git_link=loc-369-ginger.pml
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
  chan c18 = [100] of {int};
  chan c19 = [1] of {int};
  chan c20 = [0] of {int};
  chan c21 = [0] of {int};
  run wg_monitor(w17);
  run fun4(c21, c18, c19, c20, w17, child_16);
  run receiver(child_16);
  for(y12 : 0 .. (30) - (1)) {
    for20: skip;
    run fun6(c18, w17, c21, c19, c20, child_15);
    run receiver(child_15);
    for20_end: skip;
  };
  for20_exit: skip;
  c21?0;
  w17.wait?0;
  c19?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c22; chan c23; chan c24; chan c25; Wgdef w26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  if
  :: true ->
    if
    :: true ->
      c24!0;
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  run fun5(c23, w26, child_33);
  child_33?0;
  c24!0;
  stop_process: skip;
  child_27!0;
}
proctype fun5(chan c36; Wgdef w37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  int y44 = x1;
  for(y41 : 0 .. (x2) - (1)) {
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
    w37.update!y44;
    w37.update_ack?y40;
    assert y40;
    if
    :: ((y44) - (1)) != (-(3)) ->
      for(y41 : 0 .. (y44) - (1)) {
        for11: skip;
        c36!0;
        for11_end: skip;
      };
      for11_exit: skip;
    :: else  ->
      do
      :: true ->
        for12: skip;
        c36!0;
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
  child_38!0;
}
proctype fun6(chan c48; Wgdef w49; chan c50; chan c51; chan c52; chan child_53) {
  bool y54 = false;
  bool y55 = false;
  int y56 = 0;
  bool y57 = true;
  int y58 = 0;
  for21_exit: skip;
  stop_process: skip;
  child_53!0;
}
proctype fun7(chan c59; chan c60; chan c61; chan c62; Wgdef w63; chan child_64) {
  bool y65 = false;
  bool y66 = false;
  int y67 = 0;
  bool y68 = true;
  int y69 = 0;
  defer1: skip;
  skip;
  w63.update!-(1);
  w63.update_ack?y66;
  assert y66;
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