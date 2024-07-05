// git_link=loc-551-ginger.pml
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
  Mutexdef mu16 ;
  Wgdef w17 ;
  Mutexdef mu18 ;
  int y19 = -(2);
  run mutex_monitor(mu18);
  run wg_monitor(w17);
  run mutex_monitor(mu16);
  if
  :: ((0) != (-(2))) && (((y19) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y19) - (1)) {
      for11: skip;
      run fun2(w17, mu16, child_14);
      child_14?0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      run fun2(w17, mu16, child_15);
      child_15?0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  run fun5(w17, mu16, child_13);
  child_13?0;
  stop_process: skip;
  child_7!0;
}
proctype fun2(Wgdef w21; Mutexdef mu22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  chan child_29 = [1] of {int};
  chan child_30 = [1] of {int};
  if
  :: true ->
    run fun3(w21, mu22, child_30);
    child_30?0;
    goto stop_process;
  :: true ->


  fi;
  w21.update!1;
  w21.update_ack?y25;
  assert y25;
  run fun4(w21, mu22, child_29);
  run receiver(child_29);
  stop_process: skip;
  child_23!0;
}
proctype fun3(Wgdef w32; Mutexdef mu33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  mu33.Lock?y36;
  assert y36;
  defer1: skip;
  skip;
  mu33.Unlock?y36;
  assert y36;
  stop_process: skip;
  child_34!0;
}
proctype fun4(Wgdef w40; Mutexdef mu41; chan child_42) {
  bool y43 = false;
  bool y44 = false;
  int y45 = 0;
  bool y46 = true;
  int y47 = 0;
  chan child_48 = [1] of {int};
  defer2: skip;
  skip;
  if
  :: true ->
    run fun3(w40, mu41, child_48);
    child_48?0;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w40.update!-(1);
  w40.update_ack?y44;
  assert y44;
  stop_process: skip;
  child_42!0;
}
proctype fun5(Wgdef w50; Mutexdef mu51; chan child_52) {
  bool y53 = false;
  bool y54 = false;
  int y55 = 0;
  bool y56 = true;
  int y57 = 0;
  w50.wait?0;
  goto stop_process;
  stop_process: skip;
  child_52!0;
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