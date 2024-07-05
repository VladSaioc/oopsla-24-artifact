// git_link=loc-112-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  int y14 = x2;
  int y15 = x1;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: (y15) == (1) ->
    goto stop_process;
  :: else  ->
  fi;
  chan c20 = [1] of {int};
  run wg_monitor(w13);
  w13.update!(y15) - (1);
  w13.update_ack?y8;
  assert y8;
  for(y9 : y14 .. y15) {
    for10: skip;
    run fun4(w13, c20, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  run close(c20);
  c20?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(Wgdef w21; chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  int y29 = -(2);
  if
  :: true ->
    c22!0;
  :: true ->
    if
    :: true ->
      c22!0;

    fi;

  fi;
  defer1: skip;
  skip;
  w21.update!-(1);
  w21.update_ack?y25;
  assert y25;
  stop_process: skip;
  child_23!0;
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