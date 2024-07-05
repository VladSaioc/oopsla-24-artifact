// git_link=loc-444-ginger.pml
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
  chan child_3 = [1] of {int};
  run fun2(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  Mutexdef mu10 ;
  int y11 = x1;
  run mutex_monitor(mu10);
  if
  :: ((y11) - (1)) != (-(3)) ->
    for(y7 : 0 .. (y11) - (1)) {
      for10: skip;
      mu10.Lock?y6;
      assert y6;
      mu10.Unlock?y6;
      assert y6;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      mu10.Lock?y6;
      assert y6;
      mu10.Unlock?y6;
      assert y6;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_4!0;
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
