// git_link=loc-176-ginger.pml
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
  chan child_12 = [1] of {int};
  int y13 = 2;
  chan c14 = [y13] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun2(c14, child_12);
  run receiver(child_12);
  run fun3(c14, child_11);
  run receiver(child_11);
  if
  :: ((0) != (-(2))) && (((y13) - (1)) != (-(3))) ->
    for(y8 : 0 .. (y13) - (1)) {
      for11: skip;
      c14?0;
      if
      :: true ->
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
      c14?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
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
proctype fun2(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  if
  :: true ->
    c18!0;
    goto stop_process;
  :: true ->


  fi;
  c18!0;
  stop_process: skip;
  child_19!0;
}
proctype fun3(chan c26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  c26!0;
  stop_process: skip;
  child_27!0;
}
proctype receiver(chan c) {
  c?0;
}