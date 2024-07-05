// git_link=loc-61-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun3(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  int y16 = x2;
  int y17 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w15);
  chan c19 = [y17] of {int};
  chan c20 = [0] of {int};
  for(y10 : 0 .. (y17) - (1)) {
    for10: skip;
    w15.update!1;
    w15.update_ack?y9;
    assert y9;
    run fun4(c19, c20, w15, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  run fun5(c19, c20, w15, child_13);
  run receiver(child_13);
  if
  :: c20?0 ->
    break;
  :: c19?0 ->
    goto stop_process;

  fi;
  for20_exit: skip;
  for20_end: skip;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c22; chan c23; Wgdef w24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  if
  :: true ->
    if
    :: true ->
      c22!0;
    :: true ->
      c22!0;

    fi;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w24.update!-(1);
  w24.update_ack?y27;
  assert y27;
  stop_process: skip;
  child_25!0;
}
proctype fun5(chan c33; chan c34; Wgdef w35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  w35.wait?0;
  run close(c34);
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