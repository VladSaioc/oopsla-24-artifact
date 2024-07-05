// git_link=loc-54-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  chan child_9 = [1] of {int};
  run fun4(x1, child_9);
  child_9?0;
  stop_process: skip;
}
proctype fun4(int y10; chan child_11) {
  bool y12 = false;
  bool y13 = false;
  int y14 = 0;
  bool y15 = true;
  int y16 = 0;
  chan child_17 = [1] of {int};
  chan child_18 = [1] of {int};
  chan child_19 = [1] of {int};
  chan child_20 = [1] of {int};
  chan child_21 = [1] of {int};
  chan c22 = [2] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: (x2) == (0) ->
    goto stop_process;
  :: else  ->
  fi;
  chan c25 = [x2] of {int};
  chan c26 = [x3] of {int};
  run fun5(c25, c22, child_21);
  run receiver(child_21);
  run fun8(c26, c22, child_19);
  run receiver(child_19);
  run fun7(c26, c22, child_18);
  run receiver(child_18);
  goto stop_process;
  stop_process: skip;
  child_11!0;
}
proctype fun5(chan c27; chan c28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  chan child_35 = [1] of {int};
  if
  :: ((x2) - (1)) != (-(3)) ->
    for(y32 : 0 .. (x2) - (1)) {
      for20: skip;
      c27!0;
      c27?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c27!0;
      c27?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  defer1: skip;
  skip;
  run fun6(c28, child_35);
  child_35?0;
  stop_process: skip;
  child_29!0;
}
proctype fun6(chan c37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  stop_process: skip;
  if
  :: true ->
    c37!0;
  :: true ->


  fi;
  child_38!0;
}
proctype fun7(chan c45; chan c46; chan child_47) {
  bool y48 = false;
  bool y49 = false;
  int y50 = 0;
  bool y51 = true;
  int y52 = 0;
  chan child_53 = [1] of {int};
  int y54 = -(2);
  if
  :: ((x3) - (1)) != (-(3)) ->
    for(y50 : 0 .. (x3) - (1)) {
      for20: skip;
      c45!0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c45!0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  defer1: skip;
  skip;
  run fun6(c46, child_53);
  child_53?0;
  stop_process: skip;
  child_47!0;
}
proctype fun8(chan c56; chan c57; chan child_58) {
  bool y59 = false;
  bool y60 = false;
  int y61 = 0;
  bool y62 = true;
  int y63 = 0;
  chan child_64 = [1] of {int};
  defer1: skip;
  skip;
  if
  :: ((x3) - (1)) != (-(3)) ->
    for(y61 : 0 .. (x3) - (1)) {
      for20: skip;
      c56?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c56?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  defer1: skip;
  skip;
  run fun6(c57, child_64);
  child_64?0;
  stop_process: skip;
  child_58!0;
}
proctype receiver(chan c) {
  c?0;
}