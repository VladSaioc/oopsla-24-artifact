// git_link=loc-109-ginger.pml
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
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  int y17 = x2;
  int y18 = x1;
  int y19 = -(2);
  chan c20 = [1] of {int};
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
  run fun4(c20, child_16);
  run receiver(child_16);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c25 = [1] of {int};
  run fun5(c25, child_15);
  run receiver(child_15);
  if
  :: true ->
    chan c26 = [y17] of {int};
    for(y11 : 0 .. (y17) - (1)) {
      for40: skip;
      run fun6(c26, child_14);
      run receiver(child_14);
      for40_end: skip;
    };
    for40_exit: skip;
    if
    :: ((0) != (-(2))) && (((y17) - (1)) != (-(3))) ->
      for(y11 : 0 .. (y17) - (1)) {
        for51: skip;
        c26?0;
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;
        for51_end: skip;
      };
      for51_exit: skip;
    :: else  ->
      do
      :: true ->
        for50: skip;
        c26?0;
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;
        for50_end: skip;
      :: true ->
        break;

      od;
      for50_exit: skip;
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
  child_8!0;
}
proctype fun4(chan c31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  c31!0;
  stop_process: skip;
  child_32!0;
}
proctype fun5(chan c38; chan child_39) {
  bool y40 = false;
  bool y41 = false;
  int y42 = 0;
  bool y43 = true;
  int y44 = 0;
  c38!0;
  stop_process: skip;
  child_39!0;
}
proctype fun6(chan c45; chan child_46) {
  bool y47 = false;
  bool y48 = false;
  int y49 = 0;
  bool y50 = true;
  int y51 = 0;
  c45!0;
  stop_process: skip;
  child_46!0;
}
proctype receiver(chan c) {
  c?0;
}