// git_link=loc-232-ginger.pml
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
  chan child_13 = [1] of {int};
  int y14 = 0;
  int y15 = x1;
  int y16 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [1] of {int};
  chan c19 = [1] of {int};
  for(y9 : 0 .. (y14) - (1)) {
    for20: skip;
    run fun3(c18, c19, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  for(y9 : 0 .. (y15) - (1)) {
    for30: skip;
    run fun4(c18, c19, child_12);
    run receiver(child_12);
    for30_end: skip;
  };
  for30_exit: skip;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c21; chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  if
  :: true ->
    c22!0;
    goto stop_process;
  :: true ->


  fi;
  c21!0;
  stop_process: skip;
  child_23!0;
}
proctype fun4(chan c30; chan c31; chan child_32) {
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
  c30!0;
  stop_process: skip;
  child_32!0;
}
proctype receiver(chan c) {
  c?0;
}