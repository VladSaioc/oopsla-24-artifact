// git_link=loc-148-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun3(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun3(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  int y15 = x1;
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  Mutexdef mu18 ;
  Mutexdef mu19 ;
  run mutex_monitor(mu19);
  run mutex_monitor(mu18);
  run fun4(mu18, child_17);
  child_17?0;
  run fun4(mu18, child_16);
  child_16?0;
  run fun5(mu18, y15, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Mutexdef mu20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  stop_process: skip;
  child_21!0;
}
proctype fun5(Mutexdef mu27; int y28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  chan child_35 = [1] of {int};
  Wgdef w36 ;
  int y37 = x2;
  run wg_monitor(w36);
  for(y32 : 0 .. (y28) - (1)) {
    for10: skip;
    w36.update!1;
    w36.update_ack?y31;
    assert y31;
    run fun6(w36, mu27, child_35);
    run receiver(child_35);
    for10_end: skip;
  };
  for10_exit: skip;
  w36.wait?0;
  goto stop_process;
  stop_process: skip;
  child_29!0;
}
proctype fun6(Wgdef w38; Mutexdef mu39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  defer1: skip;
  skip;
  w38.update!-(1);
  w38.update_ack?y42;
  assert y42;
  stop_process: skip;
  child_40!0;
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