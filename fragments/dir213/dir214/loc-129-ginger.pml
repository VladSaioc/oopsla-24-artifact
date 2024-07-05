// git_link=loc-129-ginger.pml
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
  int y14 = -(2);
  chan c15 = [2] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    run fun2(c15, child_13);
    run receiver(child_13);
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun3(c15, y14, child_12);
  child_12?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun2(chan c20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  if
  :: true ->
    c20!0;
    c20!0;
  :: true ->
    c20!0;
    c20!0;

  fi;
  run close(c20);
  stop_process: skip;
  child_21!0;
}
proctype fun3(chan c28; int y29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  chan child_36 = [1] of {int};
  int y37 = 0;
  chan child_38 = [1] of {int};
  int y39 = 0;
  int y40 = -(2);
  if
  :: ((y29) - (1)) != (-(3)) ->
    for(y33 : 0 .. (y29) - (1)) {
      for10: skip;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      if
      :: true ->
        goto for12_end;
      :: true ->


      fi;
      if
      :: true ->
        run fun4(c28, child_36);
        child_36?0;
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;
      :: true ->


      fi;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for15: skip;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      if
      :: true ->
        goto for17_end;
      :: true ->


      fi;
      if
      :: true ->
        run fun4(c28, child_38);
        child_38?0;
        if
        :: true ->
          goto stop_process;
        :: true ->


        fi;
      :: true ->


      fi;
      for15_end: skip;
    :: true ->
      break;

    od;
    for15_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_30!0;
}
proctype fun4(chan c46; chan child_47) {
  bool y48 = false;
  bool y49 = false;
  int y50 = 0;
  bool y51 = true;
  int y52 = 0;
  c46?0;
  c46?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_47!0;
}
proctype receiver(chan c) {
  c?0;
}