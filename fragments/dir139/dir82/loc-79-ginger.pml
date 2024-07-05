// git_link=loc-79-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  int y12 = x1;
  int y13 = -(2);
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
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c17 = [y12] of {int};
  for(y8 : 0 .. (y12) - (1)) {
    for20: skip;
    run fun3(c17, child_11);
    run receiver(child_11);
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: ((y12) - (1)) != (-(3)) ->
    for(y8 : 0 .. (y12) - (1)) {
      for30: skip;
      c17?0;
      if
      :: true ->
        goto for30_end;
      :: true ->


      fi;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for30_end: skip;
    };
    for30_exit: skip;
  :: else  ->
    do
    :: true ->
      for31: skip;
      c17?0;
      if
      :: true ->
        goto for31_end;
      :: true ->


      fi;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for31_end: skip;
    :: true ->
      break;

    od;
    for31_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  c21!0;
  stop_process: skip;
  child_22!0;
}
proctype receiver(chan c) {
  c?0;
}