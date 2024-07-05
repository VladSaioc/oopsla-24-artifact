// git_link=loc-359-ginger.pml
#define  default true
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
  run fun1(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun1(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  chan c12 = [0] of {int};
  run fun2(c12, child_11);
  run receiver(child_11);
  c12?0;
  c12?0;
  stop_process: skip;
  child_5!0;
}
proctype fun2(chan c13; chan child_14) {
  bool y15 = false;
  bool y16 = false;
  int y17 = 0;
  bool y18 = true;
  int y19 = 0;
  chan child_20 = [1] of {int};
  run fun3(c13, child_20);
  child_20?0;
  stop_process: skip;
  child_14!0;
}
proctype fun3(chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  int y28 = -(2);
  if
  :: ((y28) - (1)) != (-(3)) ->
    for(y25 : 0 .. (y28) - (1)) {
      for10: skip;
      if
      :: true ->
        c21!0;
      :: true ->


      fi;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      if
      :: true ->
        c21!0;
      :: true ->


      fi;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_22!0;
}
proctype receiver(chan c) {
  c?0;
}