// git_link=loc-304-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
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
  Mutexdef mu12 ;
  Wgdef w13 ;
  int y14 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w13);
  run mutex_monitor(mu12);
  w13.update!y14;
  w13.update_ack?y7;
  assert y7;
  for(y8 : 0 .. (y14) - (1)) {
    for10: skip;
    run fun3(w13, mu12, child_11);
    run receiver(child_11);
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(Wgdef w16; Mutexdef mu17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  if
  :: true ->
    mu17.Lock?y20;
    assert y20;
    mu17.Unlock?y20;
    assert y20;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w16.update!-(1);
  w16.update_ack?y20;
  assert y20;
  stop_process: skip;
  child_18!0;
}
proctype wg_monitor(Wgdef wg) {
  int i = 0;
  end: skip;
  do
  :: wg.update?i ->
    wg.Counter = (wg.Counter) + (i);
    wg.update_ack!(wg.Counter) >= (0);
  :: (wg.Counter) == (0) ->
    end1: skip;
    if
    :: wg.update?i ->
      wg.Counter = (wg.Counter) + (i);
      wg.update_ack!(wg.Counter) >= (0);
    :: wg.wait!0 ->


    fi;

  od;
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
proctype receiver(chan c) {
  c?0;
}