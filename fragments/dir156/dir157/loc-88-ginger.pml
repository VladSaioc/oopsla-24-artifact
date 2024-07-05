// git_link=loc-88-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun1(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun1(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  int y15 = 1;
  int y16 = 4;
  int y17 = 1;
  int y18 = ((y17) + (y16)) + (y15);
  int y19 = 256;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  chan c23 = [y19] of {int};
  chan c24 = [y19] of {int};
  chan c25 = [y18] of {int};
  for(y9 : 0 .. (y17) - (1)) {
    for10: skip;
    run fun2(c23, c24, c25, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  for(y9 : 0 .. (y16) - (1)) {
    for20: skip;
    run fun3(c24, c25, c23, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  for(y9 : 0 .. (y15) - (1)) {
    for30: skip;
    run fun4(c23, c24, c25, child_12);
    run receiver(child_12);
    for30_end: skip;
  };
  for30_exit: skip;
  for(y9 : 0 .. (y18) - (1)) {
    for40: skip;
    c25?0;
    if
    :: true ->
      if
      :: (y17) == (0) ->
        run close(c23);
      :: else  ->
      fi;
    :: true ->
      if
      :: (y16) == (0) ->
        run close(c24);
      :: else  ->
      fi;
    :: true ->


    fi;
    for40_end: skip;
  };
  for40_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun2(chan c29; chan c30; chan c31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  defer1: skip;
  skip;
  c31!0;
  stop_process: skip;
  child_32!0;
}
proctype fun3(chan c38; chan c39; chan c40; chan child_41) {
  bool y42 = false;
  bool y43 = false;
  int y44 = 0;
  bool y45 = true;
  int y46 = 0;
  defer1: skip;
  skip;
  c39!0;
  stop_process: skip;
  child_41!0;
}
proctype fun4(chan c47; chan c48; chan c49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  defer1: skip;
  skip;
  c49!0;
  stop_process: skip;
  child_50!0;
}
proctype receiver(chan c) {
  c?0;
}