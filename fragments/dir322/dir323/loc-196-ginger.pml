// git_link=loc-196-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  chan child_11 = [0] of {int};
  bool y12 = true;
  int y13 = 0;
  Wgdef w14 ;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w14);
  chan c16 = [y6] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    w14.update!1;
    w14.update_ack?y9;
    run fun3(c16, child_11);
    assert y9;
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y6) - (1)) {
      for21: skip;
      run fun4(c16, w14, child_11);
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      run fun4(c16, w14, child_11);
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c18; chan child_19) {
  c18!0;
  stop_process: skip;
  child_19!0;
}
proctype fun4(chan c20; Wgdef w21; chan child_22) {
  c20?0;
  w21.update!-(1);
  stop_process: skip;
  child_22!0;
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