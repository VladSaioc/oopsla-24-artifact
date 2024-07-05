// git_link=loc-471-ginger.pml
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
  Mutexdef mu14 ;
  Wgdef w15 ;
  int y16 = x1;
  run wg_monitor(w15);
  run mutex_monitor(mu14);
  for(y9 : 0 .. (y16) - (1)) {
    for10: skip;
    if
    :: true ->
      run fun3(mu14, child_13);
      child_13?0;
      goto for10_end;
    :: true ->


    fi;
    w15.update!1;
    w15.update_ack?y8;
    assert y8;
    run fun4(w15, mu14, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(Mutexdef mu18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  mu18.Lock?y21;
  assert y21;
  defer1: skip;
  skip;
  mu18.Unlock?y21;
  assert y21;
  stop_process: skip;
  child_19!0;
}
proctype fun4(Wgdef w25; Mutexdef mu26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  run fun3(mu26, child_33);
  child_33?0;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
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