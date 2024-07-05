// git_link=loc-97-ginger.pml
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
init {
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  int y12 = x1;
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
  chan c15 = [y12] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  for(y8 : 0 .. (y12) - (1)) {
    for10: skip;
    run fun3(c15, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y12) - (1)) != (-(3)) ->
    for(y8 : 0 .. (y12) - (1)) {
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  if
  :: true ->
    c19!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c19!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c19!0;
    goto stop_process;
  :: true ->


  fi;
  c19!0;
  goto stop_process;
  stop_process: skip;
  child_20!0;
}
proctype receiver(chan c) {
  c?0;
}