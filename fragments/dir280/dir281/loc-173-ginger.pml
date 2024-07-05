// git_link=loc-173-ginger.pml
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
  chan c13 = [y5] of {int};
  if
  :: ((0) != (-(2))) && (((y5) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y5) - (1)) {
      for11: skip;
      c13!0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      c13!0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  chan c15 = [y6] of {int};
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y6) - (1)) {
      for21: skip;
      c15!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c15!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}