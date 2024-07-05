// git_link=loc-352-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun3(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  int y16 = x2;
  chan c17 = [0] of {int};
  run fun4(c17, y7, child_15);
  run receiver(child_15);
  chan c18 = [0] of {int};
  for(y11 : 0 .. (y16) - (1)) {
    for10: skip;
    run fun5(c17, c18, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y16) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y16) - (1)) {
      for21: skip;
      c18?0;
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
      c18?0;
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
  if
  :: c17?0 ->
    break;
  :: true ->
    break;

  fi;
  for30_exit: skip;
  for30_end: skip;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c22; int y23; chan child_24) {
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
  stop_process: skip;
  child_24!0;
}
proctype fun5(chan c32; chan c33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  c33!0;
  stop_process: skip;
  child_34!0;
}
proctype receiver(chan c) {
  c?0;
}