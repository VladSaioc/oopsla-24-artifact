// git_link=loc-453-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  run fun4(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  Mutexdef mu14 ;
  Wgdef w15 ;
  int y16 = ((x1) * (x2)) * (x3);
  if
  :: true ->
    x1 = 1;
  :: true ->


  fi;
  if
  :: true ->
    x2 = 1;
  :: true ->


  fi;
  if
  :: true ->
    x3 = 1;
  :: true ->


  fi;
  run wg_monitor(w15);
  w15.update!y16;
  w15.update_ack?y9;
  assert y9;
  run mutex_monitor(mu14);
  for(y10 : 0 .. (x2) - (1)) {
    for10: skip;
    for(y10 : 0 .. (x1) - (1)) {
      for11: skip;
      for(y10 : 0 .. (x3) - (1)) {
        for12: skip;
        run fun5(w15, mu14, child_13);
        run receiver(child_13);
        for12_end: skip;
      };
      for12_exit: skip;
      for11_end: skip;
    };
    for11_exit: skip;
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun5(Wgdef w21; Mutexdef mu22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  int y29 = -(2);
  mu22.Lock?y25;
  assert y25;
  defer2: skip;
  skip;
  mu22.Unlock?y25;
  assert y25;
  defer1: skip;
  skip;
  w21.update!-(1);
  w21.update_ack?y25;
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