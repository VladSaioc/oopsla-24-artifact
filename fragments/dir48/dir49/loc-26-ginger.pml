// git_link=loc-26-ginger.pml
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
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  Mutexdef mu15 ;
  int y16 = x1;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  run mutex_monitor(mu15);
  run wg_monitor(w14);
  w14.update!1;
  w14.update_ack?y8;
  assert y8;
  run fun3(w14, mu15, child_13);
  run receiver(child_13);
  for(y9 : 0 .. (y16) - (1)) {
    for10: skip;
    w14.update!1;
    w14.update_ack?y8;
    assert y8;
    run fun4(w14, mu15, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Wgdef w19; Mutexdef mu20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  if
  :: true ->
    mu20.Lock?y23;
    assert y23;
    mu20.Unlock?y23;
    assert y23;

  fi;
  defer1: skip;
  skip;
  w19.update!-(1);
  w19.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun4(Wgdef w28; Mutexdef mu29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  if
  :: true ->
    mu29.Lock?y32;
    assert y32;
    mu29.Unlock?y32;
    assert y32;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w28.update!-(1);
  w28.update_ack?y32;
  assert y32;
  stop_process: skip;
  child_30!0;
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