// git_link=loc-108-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun3(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun3(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  int y15 = 0;
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  int y18 = x2;
  int y19 = x1;
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
    goto stop_process;
  :: true ->


  fi;
  chan c24 = [1] of {int};
  chan c25 = [1] of {int};
  chan c26 = [1] of {int};
  run fun4(c24, c25, c26, child_17);
  run receiver(child_17);
  run fun5(c24, c25, c26, child_16);
  run receiver(child_16);
  if
  :: true ->
    y15 = y19;
    for(y11 : 0 .. (y19) - (1)) {
      for10: skip;
      run fun6(c24, c25, c26, child_14);
      run receiver(child_14);
      for10_end: skip;
    };
    for10_exit: skip;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c28; chan c29; chan c30; chan child_31) {
  bool y32 = false;
  bool y33 = false;
  int y34 = 0;
  bool y35 = true;
  int y36 = 0;
  c28!0;
  stop_process: skip;
  child_31!0;
}
proctype fun5(chan c37; chan c38; chan c39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  c38!0;
  stop_process: skip;
  child_40!0;
}
proctype fun6(chan c46; chan c47; chan c48; chan child_49) {
  bool y50 = false;
  bool y51 = false;
  int y52 = 0;
  bool y53 = true;
  int y54 = 0;
  c48!0;
  stop_process: skip;
  child_49!0;
}
proctype receiver(chan c) {
  c?0;
}