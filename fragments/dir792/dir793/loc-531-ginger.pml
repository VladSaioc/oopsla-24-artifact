// git_link=loc-531-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  chan child_6 = [1] of {int};
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  int y13 = 0;
  int y14 = 0;
  chan child_15 = [1] of {int};
  int y16 = x3;
  int y17 = x2;
  int y18 = x1;
  chan c19 = [0] of {int};
  for(y10 : 0 .. (y18) - (1)) {
    for10: skip;
    run fun5(c19, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y18) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y18) - (1)) {
      for23: skip;
      c19?0;
      if
      :: true ->
        goto for25_end;
      :: true ->


      fi;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c19?0;
      if
      :: true ->
        goto for22_end;
      :: true ->


      fi;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  outer: skip;
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
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(chan c26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  c26!0;
  stop_process: skip;
  child_27!0;
}
proctype receiver(chan c) {
  c?0;
}