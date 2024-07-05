// git_link=loc-486-ginger.pml
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
  chan c17 = [y7] of {int};
  for(y11 : 0 .. (y7) - (1)) {
    for10: skip;
    run fun5(c17, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y7) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y7) - (1)) {
      for27: skip;
      c17?0;
      if
      :: true ->
        goto for27_end;
      :: true ->


      fi;
      for27_end: skip;
    };
    for27_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c17?0;
      if
      :: true ->
        goto for20_end;
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
  :: true ->
    c20!0;

  fi;
  stop_process: skip;
  child_21!0;
}
proctype receiver(chan c) {
  c?0;
}