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
  chan child_3 = [1] of {int};
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  Mutexdef mu11 ;
  Wgdef w12 ;
  run wg_monitor(w12);
  run mutex_monitor(mu11);
  for(y7 : 0 .. (2669) - (1)) {
    for10: skip;
    w12.update!1;
    w12.update_ack?y6;
    assert y6;
    run fun2(w12, mu11, child_10);
    run receiver(child_10);
    for10_end: skip;
  };
  for10_exit: skip;
  w12.wait?0;
  stop_process: skip;
  child_4!0;
}
proctype fun2(Wgdef w13; Mutexdef mu14; chan child_15) {
  bool y16 = false;
  bool y17 = false;
  int y18 = 0;
  bool y19 = true;
  int y20 = 0;
  int y21 = -(2);
  if
  :: ((0) != (-(2))) && (((y21) - (1)) != (-(3))) ->
    for(y18 : 0 .. (y21) - (1)) {
      for12: skip;
      if
      :: true ->
        goto for12_end;
      :: true ->


      fi;
      if
      :: true ->
        mu14.Lock?y17;
        assert y17;
        mu14.Unlock?y17;
        assert y17;
      :: true ->


      fi;
      for12_end: skip;
    };
    for12_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      if
      :: true ->
        goto for11_end;
      :: true ->


      fi;
      if
      :: true ->
        mu14.Lock?y17;
        assert y17;
        mu14.Unlock?y17;
        assert y17;
      :: true ->


      fi;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  defer1: skip;
  skip;
  w13.update!-(1);
  w13.update_ack?y17;
  assert y17;
  stop_process: skip;
  child_15!0;
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
