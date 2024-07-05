// git_link=loc-218-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
#define  x4 ??
#define  x5 ??
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
  run fun6(x4, child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun6(int y9; chan child_10) {
  bool y11 = false;
  bool y12 = false;
  int y13 = 0;
  bool y14 = true;
  int y15 = 0;
  chan child_16 = [1] of {int};
  Wgdef w17 ;
  int y18 = x1;
  int y19 = x5;
  int y20 = x3;
  int y21 = x2;
  int y22 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  run wg_monitor(w17);
  w17.update!y18;
  w17.update_ack?y12;
  assert y12;
  chan c29 = [y18] of {int};
  for(y13 : 0 .. (y18) - (1)) {
    for10: skip;
    run fun7(c29, w17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y18) - (1)) != (-(3))) ->
    for(y13 : 0 .. (y18) - (1)) {
      for21: skip;
      c29?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c29?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  w17.wait?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
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
  goto stop_process;
  stop_process: skip;
  child_10!0;
}
proctype fun7(chan c34; Wgdef w35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  c34!0;
  defer1: skip;
  skip;
  w35.update!-(1);
  w35.update_ack?y38;
  assert y38;
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
