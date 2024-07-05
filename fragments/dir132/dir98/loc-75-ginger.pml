// git_link=loc-75-ginger.pml
#define  default true
#define  x1 ??
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
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
  chan child_12 = [1] of {int};
  Mutexdef mu13 ;
  int y14 = x1;
  run mutex_monitor(mu13);
  if
  :: ((0) != (-(2))) && ((((2) * (y14)) - (1)) != (-(3))) ->
    for(y8 : 0 .. ((2) * (y14)) - (1)) {
      for11: skip;
      run fun3(mu13, child_11);
      child_11?0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      run fun3(mu13, child_12);
      child_12?0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  stop_process: skip;
  child_5!0;
}
proctype fun3(Mutexdef mu16; chan child_17) {
  bool y18 = false;
  bool y19 = false;
  int y20 = 0;
  bool y21 = true;
  int y22 = 0;
  mu16.Lock?y19;
  assert y19;
  defer1: skip;
  skip;
  mu16.Unlock?y19;
  assert y19;
  stop_process: skip;
  child_17!0;
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
