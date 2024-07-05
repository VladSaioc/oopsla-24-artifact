// git_link=loc-192-ginger.pml
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
  int y14 = 0;
  int y15 = x2;
  int y16 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [0] of {int};
  for(y10 : 0 .. (y16) - (1)) {
    for10: skip;
    run fun4(c18, child_13);
    child_13?0;
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y16) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y16) - (1)) {
      for21: skip;
      c18?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c18?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c18);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(chan c21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  chan child_28 = [1] of {int};
  run fun5(c21, child_28);
  run receiver(child_28);
  stop_process: skip;
  child_22!0;
}
proctype fun5(chan c29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  int y36 = -(2);
  if
  :: ((y36) - (1)) != (-(3)) ->
    for(y33 : 0 .. (y36) - (1)) {
      for11: skip;
      if
      :: true ->
        if
        :: true ->
          goto for11_end;
        :: true ->


        fi;
        goto for11_end;
      :: true ->


      fi;
      if
      :: true ->
        c29!0;
        goto stop_process;
      :: true ->


      fi;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for12: skip;
      if
      :: true ->
        if
        :: true ->
          goto for12_end;
        :: true ->


        fi;
        goto for12_end;
      :: true ->


      fi;
      if
      :: true ->
        c29!0;
        goto stop_process;
      :: true ->


      fi;
      for12_end: skip;
    :: true ->
      break;

    od;
    for12_exit: skip;
  fi;
  if
  :: true ->
    c29!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    c29!0;
    goto stop_process;
  :: true ->


  fi;
  c29!0;
  stop_process: skip;
  child_30!0;
}
proctype receiver(chan c) {
  c?0;
}