// git_link=loc-497-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  run fun4(x1, x3, child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun4(int y8; int y9; chan child_10) {
  bool y11 = false;
  bool y12 = false;
  int y13 = 0;
  bool y14 = true;
  int y15 = 0;
  chan child_16 = [1] of {int};
  Wgdef w17 ;
  int y18 = x2;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  chan c21 = [y8] of {int};
  chan c22 = [y18] of {int};
  run wg_monitor(w17);
  for(y13 : 0 .. (y18) - (1)) {
    for10: skip;
    w17.update!1;
    w17.update_ack?y12;
    assert y12;
    run fun5(c21, c22, w17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y8) - (1)) != (-(3)) ->
    for(y13 : 0 .. (y8) - (1)) {
      for20: skip;
      c21!0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c21!0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  run close(c21);
  recieve: skip;
  defer1: skip;
  skip;
  w17.wait?0;
  stop_process: skip;
  child_10!0;
}
proctype fun5(chan c24; chan c25; Wgdef w26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  run fun6(c24, c25, child_33);
  child_33?0;
  defer1: skip;
  skip;
  w26.update!-(1);
  w26.update_ack?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
}
proctype fun6(chan c34; chan c35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  stop_process: skip;
  child_36!0;
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