// git_link=loc-484-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  int y13 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c15 = [y13] of {int};
  for(y9 : 0 .. (y13) - (1)) {
    for10: skip;
    run fun3(c15, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y13) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y13) - (1)) {
      for20: skip;
      c15?0;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c15?0;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  chan child_25 = [1] of {int};
  int y26 = -(2);
  if
  :: true ->
    c18!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      if
      :: true ->
        c18!0;
        goto stop_process;
      :: true ->


      fi;
      if
      :: true ->
        if
        :: true ->
          c18!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c18!0;
          goto stop_process;
        :: true ->


        fi;
        c18!0;
        goto stop_process;
      :: true ->


      fi;
      c18!0;
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    c18!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      c18!0;
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      c18!0;
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      c18!0;
      goto stop_process;
    :: true ->


    fi;
    c18!0;
    goto stop_process;
  :: true ->
    if
    :: true ->
      if
      :: true ->
        c18!0;
        goto stop_process;
      :: true ->


      fi;
      c18!0;
      goto stop_process;
    :: true ->
      if
      :: true ->
        run fun4(c18, child_25);
        child_25?0;
      :: true ->
        if
        :: true ->
          c18!0;
          goto stop_process;
        :: true ->
          c18!0;
          goto stop_process;

        fi;

      fi;

    fi;

  fi;
  stop_process: skip;
  child_19!0;
}
proctype fun4(chan c44; chan child_45) {
  bool y46 = false;
  bool y47 = false;
  int y48 = 0;
  bool y49 = true;
  int y50 = 0;
  if
  :: true ->
    if
    :: true ->
      c44!0;
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    c44!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->


  fi;
  if
  :: true ->
    c44!0;
    goto stop_process;
  :: true ->


  fi;
  c44!0;
  goto stop_process;
  stop_process: skip;
  child_45!0;
}
proctype receiver(chan c) {
  c?0;
}