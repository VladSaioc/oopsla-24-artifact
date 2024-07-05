// git_link=loc-152-ginger.pml
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
  Mutexdef mu16 ;
  Mutexdef mu17 ;
  run mutex_monitor(mu17);
  run mutex_monitor(mu16);
  run fun3(mu16, child_15);
  child_15?0;
  run fun4(mu16, y14, child_13);
  child_13?0;
  stop_process: skip;
  child_7!0;
}
proctype fun3(Mutexdef mu18; chan child_19) {
  bool y20 = false;
  bool y21 = false;
  int y22 = 0;
  bool y23 = true;
  int y24 = 0;
  stop_process: skip;
  child_19!0;
}
proctype fun4(Mutexdef mu25; int y26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  int y34 = 0;
  Wgdef w35 ;
  run wg_monitor(w35);
  for(y30 : 0 .. (y26) - (1)) {
    for10: skip;
    w35.update!1;
    w35.update_ack?y29;
    assert y29;
    run fun5(w35, mu25, child_33);
    run receiver(child_33);
    for10_end: skip;
  };
  for10_exit: skip;
  w35.wait?0;
  goto stop_process;
  stop_process: skip;
  child_27!0;
}
proctype fun5(Wgdef w36; Mutexdef mu37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  defer1: skip;
  skip;
  w36.update!-(1);
  w36.update_ack?y40;
  assert y40;
  stop_process: skip;
  child_38!0;
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