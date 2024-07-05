// git_link=loc-171-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun3(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  int y15 = x2;
  int y16 = x1;
  chan c17 = [y16] of {int};
  run fun4(c17, child_14);
  run receiver(child_14);
  for(y10 : y15 .. (y16) - (1)) {
    for10: skip;
    run fun5(c17, child_13);
    run receiver(child_13);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: true ->
    goto stop_process;
  :: c17?0 ->
    goto stop_process;

  fi;
  for20_exit: skip;
  for20_end: skip;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  defer1: skip;
  skip;
  c19!0;
  stop_process: skip;
  child_20!0;
}
proctype fun5(chan c26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  if
  :: true ->
    c26!0;
  :: true ->
    chan child_33 = [1] of {int};
    run fun4(c26, child_33);
    child_33?0;

  fi;
  for11_exit: skip;
  for11_end: skip;
  stop_process: skip;
  child_27!0;
}
proctype receiver(chan c) {
  c?0;
}