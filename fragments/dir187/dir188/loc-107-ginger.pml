// git_link=loc-107-ginger.pml
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
  int y13 = -(2);
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
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  chan c21 = [y12] of {int};
  for(y8 : 0 .. (y12) - (1)) {
    for20: skip;
    run fun3(c21, child_11);
    run receiver(child_11);
    for20_end: skip;
  };
  for20_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  c22!0;
  stop_process: skip;
  child_23!0;
}
proctype receiver(chan c) {
  c?0;
}