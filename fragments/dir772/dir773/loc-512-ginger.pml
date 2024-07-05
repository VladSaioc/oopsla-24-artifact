// git_link=loc-512-ginger.pml
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
  connLoop: skip;
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
  :: ((y19) - (1)) != (-(3)) ->
    for(y16 : 0 .. (y19) - (1)) {
      for10: skip;
      if
      :: true ->
        c12!0;
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
        c12!0;
      :: true ->


      fi;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  run close(c12);
  stop_process: skip;
  child_13!0;
}
proctype receiver(chan c) {
  c?0;
}