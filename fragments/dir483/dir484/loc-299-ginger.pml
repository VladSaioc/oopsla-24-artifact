// git_link=loc-299-ginger.pml
#define  default true
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
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
  chan child_11 = [1] of {int};
  int y12 = 3;
  Mutexdef mu13 ;
  run mutex_monitor(mu13);
  if
  :: ((y12) - (1)) != (-(3)) ->
    for(y7 : 0 .. (y12) - (1)) {
      for10: skip;
      run fun2(mu13, child_10);
      child_10?0;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      run fun2(mu13, child_11);
      child_11?0;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  stop_process: skip;
  child_4!0;
}
proctype fun2(Mutexdef mu15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  mu15.Lock?y18;
  assert y18;
  mu15.Unlock?y18;
  assert y18;
  goto stop_process;
  stop_process: skip;
  child_16!0;
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