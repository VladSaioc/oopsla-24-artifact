// git_link=loc-110-ginger.pml
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
  int y12 = -(2);
  if
  :: true ->
    chan c13 = [x1] of {int};
    for(y8 : 0 .. (x1) - (1)) {
      for20: skip;
      run fun3(c13, child_11);
      run receiver(child_11);
      for20_end: skip;
    };
    for20_exit: skip;
    if
    :: ((0) != (-(2))) && (((x1) - (1)) != (-(3))) ->
      for(y8 : 0 .. (x1) - (1)) {
        for31: skip;
        c13?0;
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;
        for31_end: skip;
      };
      for31_exit: skip;
    :: else  ->
      do
      :: true ->
        for30: skip;
        c13?0;
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
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  c17!0;
  stop_process: skip;
  child_18!0;
}
proctype receiver(chan c) {
  c?0;
}