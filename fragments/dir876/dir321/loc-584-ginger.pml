// git_link=loc-584-ginger.pml
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
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  run wg_monitor(w15);
  chan c16 = [0] of {int};
  chan c17 = [0] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    w15.update!1;
    w15.update_ack?y9;
    assert y9;
    run fun3(c17, c16, w15, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun4(c16, c17, w15, child_13);
  run receiver(child_13);
  if
  :: c16?0 ->
    break;
  :: c17?0 ->
    goto stop_process;

  fi;
  for20_exit: skip;
  for20_end: skip;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c19; chan c20; Wgdef w21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  if
  :: true ->
    c19!0;
  :: true ->


  fi;
  w21.update!-(1);
  w21.update_ack?y24;
  assert y24;
  stop_process: skip;
  child_22!0;
}
proctype fun4(chan c29; chan c30; Wgdef w31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  w31.wait?0;
  run close(c29);
  stop_process: skip;
  child_32!0;
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