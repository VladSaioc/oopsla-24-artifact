// git_link=loc-367-ginger.pml
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
  int y11 = 0;
  int y12 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c14 = [y11] of {int};
  for(y7 : 0 .. (y11) - (1)) {
    for20: skip;
    run fun2(c14, child_10);
    run receiver(child_10);
    for20_end: skip;
  };
  for20_exit: skip;
  if
  :: ((y11) - (1)) != (-(3)) ->
    for(y7 : 0 .. (y11) - (1)) {
      for30: skip;
      c14?0;
      if
      :: true ->
        goto for30_end;
      :: true ->
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;

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
      c14?0;
      if
      :: true ->
        goto for31_end;
      :: true ->
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;

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
  child_4!0;
}
proctype fun2(chan c19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  c19!0;
  stop_process: skip;
  child_20!0;
}
proctype receiver(chan c) {
  c?0;
}