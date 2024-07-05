// git_link=loc-121-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  int y13 = x1;
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
  chan c16 = [0] of {int};
  run fun3(c16, y13, child_12);
  run receiver(child_12);
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
  c16?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c20; int y21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  chan child_28 = [1] of {int};
  chan c29 = [y21] of {int};
  for(y25 : 0 .. (y21) - (1)) {
    for10: skip;
    run fun4(c29, child_28);
    run receiver(child_28);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y21) - (1)) != (-(3))) ->
    for(y25 : 0 .. (y21) - (1)) {
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
  c20!0;
  stop_process: skip;
  child_22!0;
}
proctype fun4(chan c31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  if
  :: true ->
    c31!0;
    goto stop_process;
  :: true ->


  fi;
  c31!0;
  stop_process: skip;
  child_32!0;
}
proctype receiver(chan c) {
  c?0;
}