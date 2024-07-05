// git_link=loc-354-ginger.pml
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
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = y6;
  int y15 = -(2);
  chan c16 = [y14] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for20: skip;
    run fun3(c16, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  chan child_25 = [1] of {int};
  run fun4(c18, child_25);
  child_25?0;
  stop_process: skip;
  child_19!0;
}
proctype fun4(chan c26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      c26!0;
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      c26!0;
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    c26!0;
    goto stop_process;
  :: true ->


  fi;
  c26!0;
  stop_process: skip;
  child_27!0;
}
proctype receiver(chan c) {
  c?0;
}