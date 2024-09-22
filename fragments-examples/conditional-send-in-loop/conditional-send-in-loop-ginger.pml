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
init {
  chan child_3 = [1] of {int};
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  int y11 = 0;
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
  chan c15 = [0] of {int};
  for(y7 : 0 .. (y11) - (1)) {
    for10: skip;
    run fun2(c15, child_10);
    run receiver(child_10);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y11) - (1)) != (-(3))) ->
    for(y7 : 0 .. (y11) - (1)) {
      for21: skip;
      c15?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c15?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
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
  goto stop_process;
  stop_process: skip;
  child_18!0;
}
proctype receiver(chan c) {
  c?0;
}
