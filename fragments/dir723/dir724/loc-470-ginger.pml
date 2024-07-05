// git_link=loc-470-ginger.pml
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
  run fun2(x1, child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(int y4; chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan c11 = [y4] of {int};
  if
  :: ((y4) - (1)) != (-(3)) ->
    for(y8 : 0 .. (y4) - (1)) {
      for11: skip;
      c11!0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      c11!0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}