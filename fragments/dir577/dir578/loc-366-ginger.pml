// git_link=loc-366-ginger.pml
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
init {
  chan child_5 = [1] of {int};
  run fun3(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = x2;
  int y15 = -(2);
  chan c16 = [y14] of {int};
  chan c17 = [y14] of {int};
  for(y10 : 0 .. (y14) - (1)) {
    for20: skip;
    run fun4(c16, c17, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
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
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c20; chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  if
  :: true ->
    c21!0;
    goto stop_process;
  :: true ->


  fi;
  c20!0;
  stop_process: skip;
  child_22!0;
}
proctype receiver(chan c) {
  c?0;
}