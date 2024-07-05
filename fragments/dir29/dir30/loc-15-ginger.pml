// git_link=loc-15-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun4(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  int y15 = 0;
  Wgdef w16 ;
  int y17 = y7;
  int y18 = x3;
  int y19 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c21 = [y17] of {int};
  chan c22 = [30] of {int};
  run wg_monitor(w16);
  for(y11 : 0 .. (y19) - (1)) {
    for20: skip;
    for(y11 : 0 .. (y18) - (1)) {
      for21: skip;
      if
      :: true ->
        goto for21_end;
      :: true ->


      fi;
      w16.update!1;
      w16.update_ack?y10;
      assert y10;
      run fun5(c21, c22, w16, child_14);
      run receiver(child_14);
      y15 = (y15) + (1);
      for21_end: skip;
    };
    for21_exit: skip;
    for20_end: skip;
  };
  for20_exit: skip;
  w16.wait?0;
  if
  :: ((0) != (-(2))) && (((y15) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y15) - (1)) {
      for31: skip;
      c21?0;
      for31_end: skip;
    };
    for31_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c21?0;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(chan c25; chan c26; Wgdef w27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  c26!0;
  c25!0;
  w27.update!-(1);
  w27.update_ack?y30;
  assert y30;
  c26?0;
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