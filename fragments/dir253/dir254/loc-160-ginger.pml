// git_link=loc-160-ginger.pml
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
  chan child_8 = [1] of {int};
  run fun3(child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun3(chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  int y18 = x2;
  int y19 = x1;
  chan c20 = [y19] of {int};
  chan c21 = [y19] of {int};
  for(y12 : 0 .. (y18) - (1)) {
    for10: skip;
    if
    :: true ->
      run fun4(c20, c21, child_15);
      run receiver(child_15);
    :: true ->
      run fun6(c20, c21, child_17);
      run receiver(child_17);
      run fun7(c20, c21, child_16);
      run receiver(child_16);

    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y18) - (1)) != (-(3)) ->
    for(y12 : 0 .. (y18) - (1)) {
      for20: skip;
      c20?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c20?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c24; chan c25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  chan child_32 = [1] of {int};
  run fun5(c24, child_32);
  child_32?0;
  stop_process: skip;
  child_26!0;
}
proctype fun5(chan c33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c33!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c33!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c33!0;
    goto stop_process;
  :: true ->


  fi;
  c33!0;
  stop_process: skip;
  child_34!0;
}
proctype fun6(chan c44; chan c45; chan child_46) {
  bool y47 = false;
  bool y48 = false;
  int y49 = 0;
  bool y50 = true;
  int y51 = 0;
  chan child_52 = [1] of {int};
  run fun5(c44, child_52);
  child_52?0;
  stop_process: skip;
  child_46!0;
}
proctype fun7(chan c53; chan c54; chan child_55) {
  bool y56 = false;
  bool y57 = false;
  int y58 = 0;
  bool y59 = true;
  int y60 = 0;
  chan child_61 = [1] of {int};
  run fun5(c54, child_61);
  child_61?0;
  stop_process: skip;
  child_55!0;
}
proctype receiver(chan c) {
  c?0;
}