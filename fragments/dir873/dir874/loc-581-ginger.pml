// git_link=loc-581-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun1(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun1(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  int y14 = 2;
  run wg_monitor(w13);
  chan c15 = [0] of {int};
  chan c16 = [0] of {int};
  for(y8 : 0 .. (y14) - (1)) {
    for10: skip;
    w13.update!1;
    w13.update_ack?y7;
    assert y7;
    run fun2(c15, c16, w13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun3(c15, c16, w13, child_11);
  run receiver(child_11);
  if
  :: c15?0 ->
    break;
  :: c16?0 ->
    goto stop_process;

  fi;
  for20_exit: skip;
  for20_end: skip;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun2(chan c18; chan c19; Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  if
  :: true ->

  :: true ->


  fi;
  if
  :: true ->
    c19!0;
  :: true ->


  fi;
  if
  :: true ->
    c19!0;
  :: true ->


  fi;
  w20.update!-(1);
  w20.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun3(chan c30; chan c31; Wgdef w32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  w32.wait?0;
  run close(c30);
  stop_process: skip;
  child_33!0;
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