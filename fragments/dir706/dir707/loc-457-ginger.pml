// git_link=loc-457-ginger.pml
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
  run fun4(x3, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  int y14 = 0;
  chan child_15 = [1] of {int};
  int y16 = x2;
  int y17 = x1;
  chan c18 = [y17] of {int};
  for(y11 : 0 .. (y17) - (1)) {
    for10: skip;
    run fun5(c18, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y17) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y17) - (1)) {
      for33: skip;
      c18?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for33_end: skip;
    };
    for33_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c18?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  if
  :: true ->
    if
    :: true ->
      c21!0;
    :: true ->
      c21!0;

    fi;
  :: true ->
    c21!0;

  fi;
  stop_process: skip;
  child_22!0;
}
proctype receiver(chan c) {
  c?0;
}