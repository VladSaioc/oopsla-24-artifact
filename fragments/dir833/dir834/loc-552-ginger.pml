// git_link=loc-552-ginger.pml
#define  default true
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
  run fun1(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun1(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  Mutexdef mu17 ;
  Wgdef w18 ;
  Mutexdef mu19 ;
  int y20 = -(2);
  run mutex_monitor(mu19);
  run wg_monitor(w18);
  run mutex_monitor(mu17);
  if
  :: ((0) != (-(2))) && ((((y20) - (1)) - (1)) != (-(3))) ->
    for(y10 : 0 .. ((y20) - (1)) - (1)) {
      for11: skip;
      run fun2(w18, mu17, child_15);
      child_15?0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      run fun2(w18, mu17, child_16);
      child_16?0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  run fun2(w18, mu17, child_14);
  child_14?0;
  run fun5(w18, mu17, child_13);
  child_13?0;
  stop_process: skip;
  child_7!0;
}
proctype fun2(Wgdef w22; Mutexdef mu23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  chan child_30 = [1] of {int};
  chan child_31 = [1] of {int};
  if
  :: true ->
    run fun3(w22, mu23, child_31);
    child_31?0;
    goto stop_process;
  :: true ->


  fi;
  w22.update!1;
  w22.update_ack?y26;
  assert y26;
  run fun4(w22, mu23, child_30);
  run receiver(child_30);
  stop_process: skip;
  child_24!0;
}
proctype fun3(Wgdef w33; Mutexdef mu34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  mu34.Lock?y37;
  assert y37;
  defer1: skip;
  skip;
  mu34.Unlock?y37;
  assert y37;
  stop_process: skip;
  child_35!0;
}
proctype fun4(Wgdef w41; Mutexdef mu42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  chan child_49 = [1] of {int};
  defer2: skip;
  skip;
  if
  :: true ->
    run fun3(w41, mu42, child_49);
    child_49?0;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w41.update!-(1);
  w41.update_ack?y45;
  assert y45;
  stop_process: skip;
  child_43!0;
}
proctype fun5(Wgdef w51; Mutexdef mu52; chan child_53) {
  bool y54 = false;
  bool y55 = false;
  int y56 = 0;
  bool y57 = true;
  int y58 = 0;
  w51.wait?0;
  goto stop_process;
  stop_process: skip;
  child_53!0;
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