// git_link=loc-274-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
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
  chan child_5 = [1] of {int};
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Wgdef w13 ;
  Mutexdef mu14 ;
  Mutexdef mu15 ;
  int y16 = x2;
  int y17 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run mutex_monitor(mu15);
  run mutex_monitor(mu14);
  run wg_monitor(w13);
  w13.update!y17;
  w13.update_ack?y8;
  assert y8;
  for(y9 : 0 .. (y17) - (1)) {
    for10: skip;
    run fun4(w13, mu15, mu14, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w13.wait?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(Wgdef w20; Mutexdef mu21; Mutexdef mu22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  if
  :: true ->
    mu22.Lock?y25;
    assert y25;
    mu22.Unlock?y25;
    assert y25;
  :: true ->


  fi;
  mu21.Lock?y25;
  assert y25;
  mu21.Unlock?y25;
  assert y25;
  defer1: skip;
  skip;
  w20.update!-(1);
  w20.update_ack?y25;
  assert y25;
  stop_process: skip;
  child_23!0;
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