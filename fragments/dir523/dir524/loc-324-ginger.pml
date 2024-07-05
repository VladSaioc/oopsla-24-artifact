// git_link=loc-324-ginger.pml
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
  w15.update!y6;
  w15.update_ack?y9;
  assert y9;
  chan c16 = [0] of {int};
  chan c17 = [0] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    run fun3(c16, c17, w15, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun4(c16, c17, w15, child_13);
  run receiver(child_13);
  if
  :: c17?0 ->

  :: c16?0 ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;

  fi;
  for20_exit: skip;
  for20_end: skip;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c20; chan c21; Wgdef w22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  if
  :: true ->
    if
    :: true ->
      c20!0;
    :: true ->
      if
      :: true ->
        c20!0;
      :: true ->


      fi;

    fi;
    w22.update!-(1);
    w22.update_ack?y25;
    assert y25;
  :: true ->


  fi;
  stop_process: skip;
  child_23!0;
}
proctype fun4(chan c32; chan c33; Wgdef w34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  w34.wait?0;
  run close(c33);
  stop_process: skip;
  child_35!0;
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