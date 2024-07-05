// git_link=loc-574-ginger.pml
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
    chan c18 = [y14] of {int};
    for(y9 : 0 .. (y14) - (1)) {
      for10: skip;
      run fun4(c18, child_12);
      run receiver(child_12);
      for10_end: skip;
    };
    for10_exit: skip;
    if
    :: ((y14) - (1)) != (-(3)) ->
      for(y9 : 0 .. (y14) - (1)) {
        for20: skip;
        c18?0;
        if
        :: true ->
          goto for20_end;
        :: true ->


        fi;
        for20_end: skip;
      };
      for20_exit: skip;
    :: else  ->
      do
      :: true ->
        for23: skip;
        c18?0;
        if
        :: true ->
          goto for23_end;
        :: true ->


        fi;
        for23_end: skip;
      :: true ->
        break;

      od;
      for23_exit: skip;
    fi;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  int y29 = -(2);
  c22!0;
  stop_process: skip;
  child_23!0;
}
proctype receiver(chan c) {
  c?0;
}