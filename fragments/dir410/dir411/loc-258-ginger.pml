// git_link=loc-258-ginger.pml
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
      goto for10_end;
    :: true ->


    fi;
    if
    :: true ->
      w15.update!1;
      w15.update_ack?y8;
      assert y8;
      run fun3(w15, mu14, child_13);
      run receiver(child_13);
    :: true ->


    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  w15.update!1;
  w15.update_ack?y8;
  assert y8;
  run fun4(w15, mu14, child_12);
  run receiver(child_12);
  w15.wait?0;
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
  mu20.Lock?y23;
  assert y23;
  defer2: skip;
  skip;
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
proctype fun4(Wgdef w27; Mutexdef mu28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
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