// git_link=loc-281-ginger.pml
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
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  int y16 = 3;
  Mutexdef mu17 ;
  run mutex_monitor(mu17);
  if
  :: ((y16) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y16) - (1)) {
      for10: skip;
      run fun2(mu17, child_13);
      child_13?0;
      run fun4(mu17, child_12);
      child_12?0;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for15: skip;
      run fun2(mu17, child_15);
      child_15?0;
      run fun4(mu17, child_14);
      child_14?0;
      for15_end: skip;
    :: true ->
      break;

    od;
    for15_exit: skip;
  fi;
  stop_process: skip;
  child_6!0;
}
proctype fun2(Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  chan child_26 = [1] of {int};
  chan child_27 = [1] of {int};
  int y28 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  mu19.Lock?y22;
  assert y22;
  if
  :: ((y28) - (1)) != (-(3)) ->
    for(y23 : 0 .. (y28) - (1)) {
      for11: skip;
      if
      :: true ->
        run fun3(mu19, child_26);
        child_26?0;

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
        run fun3(mu19, child_27);
        child_27?0;

      fi;
      for12_end: skip;
    :: true ->
      break;

    od;
    for12_exit: skip;
  fi;
  defer1: skip;
  skip;
  mu19.Unlock?y22;
  assert y22;
  stop_process: skip;
  child_20!0;
}
proctype fun3(Mutexdef mu32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  stop_process: skip;
  child_33!0;
}
proctype fun4(Mutexdef mu39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  chan child_46 = [1] of {int};
  chan child_47 = [1] of {int};
  int y48 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  mu39.Lock?y42;
  assert y42;
  if
  :: ((y48) - (1)) != (-(3)) ->
    for(y43 : 0 .. (y48) - (1)) {
      for13: skip;
      run fun3(mu39, child_46);
      child_46?0;
      for13_end: skip;
    };
    for13_exit: skip;
  :: else  ->
    do
    :: true ->
      for14: skip;
      run fun3(mu39, child_47);
      child_47?0;
      for14_end: skip;
    :: true ->
      break;

    od;
    for14_exit: skip;
  fi;
  defer1: skip;
  skip;
  mu39.Unlock?y42;
  assert y42;
  stop_process: skip;
  child_40!0;
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