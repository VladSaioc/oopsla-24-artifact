// git_link=loc-399-ginger.pml
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
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Mutexdef mu15 ;
  Wgdef w16 ;
  run wg_monitor(w16);
  run mutex_monitor(mu15);
  w16.update!y6;
  w16.update_ack?y9;
  assert y9;
  if
  :: true ->
    for(y10 : 0 .. (y6) - (1)) {
      for10: skip;
      run fun3(mu15, w16, child_14);
      run receiver(child_14);
      for10_end: skip;
    };
    for10_exit: skip;
  :: true ->


  fi;
  if
  :: true ->
    for(y10 : 0 .. (y6) - (1)) {
      for20: skip;
      run fun4(mu15, w16, child_13);
      run receiver(child_13);
      for20_end: skip;
    };
    for20_exit: skip;
  :: true ->


  fi;
  w16.wait?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(Mutexdef mu20; Wgdef w21; chan child_22) {
  bool y23 = false;
  bool y24 = false;
  int y25 = 0;
  bool y26 = true;
  int y27 = 0;
  if
  :: true ->
    mu20.Lock?y24;
    assert y24;
    mu20.Unlock?y24;
    assert y24;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w21.update!-(1);
  w21.update_ack?y24;
  assert y24;
  stop_process: skip;
  child_22!0;
}
proctype fun4(Mutexdef mu29; Wgdef w30; chan child_31) {
  bool y32 = false;
  bool y33 = false;
  int y34 = 0;
  bool y35 = true;
  int y36 = 0;
  if
  :: true ->
    mu29.Lock?y33;
    assert y33;
    mu29.Unlock?y33;
    assert y33;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w30.update!-(1);
  w30.update_ack?y33;
  assert y33;
  stop_process: skip;
  child_31!0;
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