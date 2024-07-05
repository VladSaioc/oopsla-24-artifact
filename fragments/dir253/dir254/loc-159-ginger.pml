// git_link=loc-159-ginger.pml
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c21 = [y19] of {int};
  chan c22 = [y19] of {int};
  for(y12 : 0 .. (y18) - (1)) {
    for10: skip;
    if
    :: true ->
      run fun4(c21, c22, child_15);
      run receiver(child_15);
    :: true ->
      run fun6(c21, c22, child_17);
      run receiver(child_17);
      run fun7(c21, c22, child_16);
      run receiver(child_16);

    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y18) - (1)) != (-(3)) ->
    for(y12 : 0 .. (y18) - (1)) {
      for20: skip;
      c21?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c21?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(chan c26; chan c27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  chan child_34 = [1] of {int};
  run fun5(c26, child_34);
  child_34?0;
  stop_process: skip;
  child_28!0;
}
proctype fun5(chan c35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c35!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c35!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c35!0;
    goto stop_process;
  :: true ->


  fi;
  c35!0;
  stop_process: skip;
  child_36!0;
}
proctype fun6(chan c46; chan c47; chan child_48) {
  bool y49 = false;
  bool y50 = false;
  int y51 = 0;
  bool y52 = true;
  int y53 = 0;
  chan child_54 = [1] of {int};
  run fun5(c46, child_54);
  child_54?0;
  stop_process: skip;
  child_48!0;
}
proctype fun7(chan c55; chan c56; chan child_57) {
  bool y58 = false;
  bool y59 = false;
  int y60 = 0;
  bool y61 = true;
  int y62 = 0;
  chan child_63 = [1] of {int};
  run fun5(c56, child_63);
  child_63?0;
  stop_process: skip;
  child_57!0;
}
proctype receiver(chan c) {
  c?0;
}