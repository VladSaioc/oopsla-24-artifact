// git_link=loc-224-ginger.pml
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
  int y13 = x2;
  int y14 = x1;
  int y15 = -(2);
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
  chan c24 = [0] of {int};
  chan c25 = [0] of {int};
  for(y9 : 0 .. (y13) - (1)) {
    for30: skip;
    run fun4(c24, c25, child_12);
    run receiver(child_12);
    for30_end: skip;
  };
  for30_exit: skip;
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
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(chan c29; chan c30; chan child_31) {
  bool y32 = false;
  bool y33 = false;
  int y34 = 0;
  bool y35 = true;
  int y36 = 0;
  if
  :: true ->
    c30!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c30!0;
    goto stop_process;
  :: true ->


  fi;
  c29!0;
  goto stop_process;
  stop_process: skip;
  child_31!0;
}
proctype receiver(chan c) {
  c?0;
}