// git_link=loc-479-ginger.pml
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
  chan child_3 = [1] of {int};
  run fun2(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  int y10 = -(2);
  int y11 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c13 = [y11] of {int};
  if
  :: ((0) != (-(2))) && (((y11) - (1)) != (-(3))) ->
    for(y7 : 0 .. (y11) - (1)) {
      for23: skip;
      c13!0;
      c13?0;
      for23_end: skip;
    };
    for23_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c13?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  stop_process: skip;
  child_4!0;
}