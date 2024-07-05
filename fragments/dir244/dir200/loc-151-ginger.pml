// git_link=loc-151-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun2(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun2(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  int y14 = x1;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  Mutexdef mu17 ;
  Mutexdef mu18 ;
  run mutex_monitor(mu18);
  run mutex_monitor(mu17);
  run fun3(mu17, child_16);
  child_16?0;
  run fun3(mu17, child_15);
  child_15?0;
  run fun4(mu17, y14, child_13);
  child_13?0;
  stop_process: skip;
  child_7!0;
}
proctype fun3(Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  stop_process: skip;
  child_20!0;
}
proctype fun4(Mutexdef mu26; int y27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  chan child_34 = [1] of {int};
  int y35 = 0;
  Wgdef w36 ;
  run wg_monitor(w36);
  for(y31 : 0 .. (y27) - (1)) {
    for10: skip;
    w36.update!1;
    w36.update_ack?y30;
    assert y30;
    run fun5(w36, mu26, child_34);
    run receiver(child_34);
    for10_end: skip;
  };
  for10_exit: skip;
  w36.wait?0;
  goto stop_process;
  stop_process: skip;
  child_28!0;
}
proctype fun5(Wgdef w37; Mutexdef mu38; chan child_39) {
  bool y40 = false;
  bool y41 = false;
  int y42 = 0;
  bool y43 = true;
  int y44 = 0;
  defer1: skip;
  skip;
  w37.update!-(1);
  w37.update_ack?y41;
  assert y41;
  stop_process: skip;
  child_39!0;
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