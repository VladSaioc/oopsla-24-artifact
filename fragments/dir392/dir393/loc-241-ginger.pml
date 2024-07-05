// git_link=loc-241-ginger.pml
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
  chan child_3 = [1] of {int};
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  chan c11 = [0] of {int};
  run fun2(c11, child_10);
  run receiver(child_10);
  c11?0;
  goto stop_process;
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c12; chan child_13) {
  bool y14 = false;
  bool y15 = false;
  int y16 = 0;
  bool y17 = true;
  int y18 = 0;
  int y19 = -(2);
  if
  :: ((1) != (-(2))) && ((y19) != (-(2))) ->
    for(y16 : 1 .. y19) {
      for11: skip;
      if
      :: true ->
        c12!0;
        goto stop_process;
      :: true ->


      fi;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      if
      :: true ->
        c12!0;
        goto stop_process;
      :: true ->


      fi;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  c12!0;
  stop_process: skip;
  child_13!0;
}
proctype receiver(chan c) {
  c?0;
}