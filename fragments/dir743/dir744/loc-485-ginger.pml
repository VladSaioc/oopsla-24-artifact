// git_link=loc-485-ginger.pml
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
  run fun4(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  int y15 = x3;
  int y16 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [y7] of {int};
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    run fun5(c18, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y7) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y7) - (1)) {
      for23: skip;
      c18?0;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c18?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c18);
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(chan c20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  if
  :: true ->
    c20!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c20!0;
    goto stop_process;
  :: true ->


  fi;
  c20!0;
  stop_process: skip;
  child_21!0;
}
proctype receiver(chan c) {
  c?0;
}