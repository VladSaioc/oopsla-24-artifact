// git_link=loc-535-ginger.pml
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
  for(y8 : 0 .. (y14) - (1)) {
    for10: skip;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    w13.update!1;
    w13.update_ack?y7;
    assert y7;
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
proctype fun3(Wgdef w18; Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  int y26 = -(2);
  mu19.Lock?y22;
  assert y22;
  defer2: skip;
  skip;
  mu19.Unlock?y22;
  assert y22;
  defer1: skip;
  skip;
  w18.update!-(1);
  w18.update_ack?y22;
  assert y22;
  stop_process: skip;
  child_20!0;
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