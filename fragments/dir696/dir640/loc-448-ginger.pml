// git_link=loc-448-ginger.pml
#define  default true
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
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
  Mutexdef mu13 ;
  Mutexdef mu14 ;
  run mutex_monitor(mu14);
  run mutex_monitor(mu13);
  run fun2(mu13, child_12);
  child_12?0;
  stop_process: skip;
  child_6!0;
}
proctype fun2(Mutexdef mu15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  chan child_22 = [1] of {int};
  chan child_23 = [1] of {int};
  chan child_24 = [1] of {int};
  chan child_25 = [1] of {int};
  int y26 = 0;
  int y27 = 0;
  chan child_28 = [1] of {int};
  chan child_29 = [1] of {int};
  chan child_30 = [1] of {int};
  chan child_31 = [1] of {int};
  int y32 = 0;
  int y33 = 0;
  chan child_34 = [1] of {int};
  int y35 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun3(mu15, child_34);
  child_34?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: ((y35) - (1)) != (-(3)) ->
    for(y19 : 0 .. (y35) - (1)) {
      for10: skip;
      if
      :: true ->
        goto for10_end;
      :: true ->


      fi;
      if
      :: ((y33) - (1)) != (-(3)) ->
        for(y19 : 0 .. (y33) - (1)) {
          for11: skip;
          if
          :: ((y32) - (1)) != (-(3)) ->
            for(y19 : 0 .. (y32) - (1)) {
              for12: skip;
              run fun4(mu15, child_22);
              child_22?0;
              for12_end: skip;
            };
            for12_exit: skip;
          :: else  ->
            do
            :: true ->
              for15: skip;
              run fun4(mu15, child_23);
              child_23?0;
              for15_end: skip;
            :: true ->
              break;

            od;
            for15_exit: skip;
          fi;
          for11_end: skip;
        };
        for11_exit: skip;
      :: else  ->
        do
        :: true ->
          for16: skip;
          if
          :: ((y32) - (1)) != (-(3)) ->
            for(y19 : 0 .. (y32) - (1)) {
              for17: skip;
              run fun4(mu15, child_24);
              child_24?0;
              for17_end: skip;
            };
            for17_exit: skip;
          :: else  ->
            do
            :: true ->
              for18: skip;
              run fun4(mu15, child_25);
              child_25?0;
              for18_end: skip;
            :: true ->
              break;

            od;
            for18_exit: skip;
          fi;
          for16_end: skip;
        :: true ->
          break;

        od;
        for16_exit: skip;
      fi;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for19: skip;
      if
      :: true ->
        goto for19_end;
      :: true ->


      fi;
      if
      :: ((y33) - (1)) != (-(3)) ->
        for(y19 : 0 .. (y33) - (1)) {
          for110: skip;
          if
          :: ((y32) - (1)) != (-(3)) ->
            for(y19 : 0 .. (y32) - (1)) {
              for111: skip;
              run fun4(mu15, child_28);
              child_28?0;
              for111_end: skip;
            };
            for111_exit: skip;
          :: else  ->
            do
            :: true ->
              for112: skip;
              run fun4(mu15, child_29);
              child_29?0;
              for112_end: skip;
            :: true ->
              break;

            od;
            for112_exit: skip;
          fi;
          for110_end: skip;
        };
        for110_exit: skip;
      :: else  ->
        do
        :: true ->
          for113: skip;
          if
          :: ((y32) - (1)) != (-(3)) ->
            for(y19 : 0 .. (y32) - (1)) {
              for114: skip;
              run fun4(mu15, child_30);
              child_30?0;
              for114_end: skip;
            };
            for114_exit: skip;
          :: else  ->
            do
            :: true ->
              for115: skip;
              run fun4(mu15, child_31);
              child_31?0;
              for115_end: skip;
            :: true ->
              break;

            od;
            for115_exit: skip;
          fi;
          for113_end: skip;
        :: true ->
          break;

        od;
        for113_exit: skip;
      fi;
      for19_end: skip;
    :: true ->
      break;

    od;
    for19_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_16!0;
}
proctype fun3(Mutexdef mu42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  mu42.Lock?y45;
  assert y45;
  defer1: skip;
  skip;
  mu42.Unlock?y45;
  assert y45;
  stop_process: skip;
  child_43!0;
}
proctype fun4(Mutexdef mu49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  mu49.Lock?y52;
  assert y52;
  defer1: skip;
  skip;
  mu49.Unlock?y52;
  assert y52;
  stop_process: skip;
  child_50!0;
}
proctype mutex_monitor(Mutexdef m) {
  bool locked = false;
  end: skip;
  do
  :: true ->
    if
    :: (m.Counter) > (0) ->
      if
      :: m.RUnlock!true ->
        m.Counter = (m.Counter) - (1);
      :: m.RLock!true ->
        m.Counter = (m.Counter) + (1);

      fi;
    :: locked ->
      m.Unlock!true;
      locked = false;
    :: else  ->
      if
      :: m.Unlock!false ->

      :: m.Lock!true ->
        locked = true;
      :: m.RUnlock!false ->

      :: m.RLock!true ->
        m.Counter = (m.Counter) + (1);

      fi;
      end1: skip;
    fi;

  od;
}