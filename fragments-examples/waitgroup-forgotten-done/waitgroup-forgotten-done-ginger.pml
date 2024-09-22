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
  chan child_4 = [1] of {int};
  run fun2(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w13);
  chan c15 = [y5] of {int};
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    w13.update!1;
    w13.update_ack?y8;
    assert y8;
    run fun3(c15, w13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  if
  :: ((y5) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y5) - (1)) {
      for20: skip;
      c15?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c15?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c17; Wgdef w18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: true ->
    c17!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c17!0;
    goto stop_process;
  :: true ->


  fi;
  c17!0;
  stop_process: skip;
  child_19!0;
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
