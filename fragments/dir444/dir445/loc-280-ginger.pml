// git_link=loc-280-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun3(x1, x2, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun3(int y5; int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  int y13 = -(2);
  chan c14 = [y5] of {int};
  if
  :: ((y6) - (1)) != (-(3)) ->
    for(y10 : 0 .. (y6) - (1)) {
      for10: skip;
      c14!0;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      c14!0;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  stop_process: skip;
  child_7!0;
}