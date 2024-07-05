// git_link=loc-513-ginger.pml
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
  chan child_12 = [1] of {int};
  int y13 = -(2);
  int y14 = -(2);
  int y15 = -(2);
  int y16 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [y16] of {int};
  chan c19 = [0] of {int};
  if
  :: ((y13) != (-(2))) && (((y14) - (1)) != (-(3))) ->
    for(y8 : y13 .. (y14) - (1)) {
      for21: skip;
      run fun3(c18, c19, child_11);
      child_11?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      run fun3(c18, c19, child_12);
      child_12?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c22; chan c23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
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
  child_24!0;
}