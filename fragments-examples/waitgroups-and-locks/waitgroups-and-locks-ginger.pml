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
  chan child_6 = [1] of {int};
  run fun3(x1, x2, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(int y7; int y8; chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  Mutexdef mu17 ;
  Wgdef w18 ;
  run wg_monitor(w18);
  run mutex_monitor(mu17);
  for(y12 : 0 .. (y7) - (1)) {
    for10: skip;
    w18.update!1;
    w18.update_ack?y11;
    assert y11;
    run fun4(w18, mu17, child_16);
    run receiver(child_16);
    for10_end: skip;
  };
  for10_exit: skip;
  for(y12 : 0 .. (y8) - (1)) {
    for20: skip;
    w18.update!1;
    w18.update_ack?y11;
    assert y11;
    run fun5(w18, mu17, child_15);
    run receiver(child_15);
    for20_end: skip;
  };
  for20_exit: skip;
  w18.wait?0;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun4(Wgdef w19; Mutexdef mu20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  mu20.Lock?y23;
  assert y23;
  mu20.Unlock?y23;
  assert y23;
  defer1: skip;
  skip;
  w19.update!-(1);
  w19.update_ack?y23;
  assert y23;
  stop_process: skip;
  child_21!0;
}
proctype fun5(Wgdef w27; Mutexdef mu28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  mu28.Lock?y31;
  assert y31;
  mu28.Unlock?y31;
  assert y31;
  defer1: skip;
  skip;
  w27.update!-(1);
  w27.update_ack?y31;
  assert y31;
  stop_process: skip;
  child_29!0;
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
