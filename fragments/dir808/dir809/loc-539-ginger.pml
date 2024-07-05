// git_link=loc-539-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
#define  x4 ??
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
  run fun5(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun5(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  Mutexdef mu15 ;
  Wgdef w16 ;
  int y17 = x4;
  int y18 = x3;
  int y19 = x2;
  int y20 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w16);
  run mutex_monitor(mu15);
  for(y11 : 0 .. (y20) - (1)) {
    for10: skip;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    for(y11 : 0 .. (y19) - (1)) {
      for11: skip;
      if
      :: true ->
        w16.update!1;
        w16.update_ack?y10;
        assert y10;
        run fun6(w16, mu15, child_14);
        run receiver(child_14);
      :: true ->


      fi;
      for11_end: skip;
    };
    for11_exit: skip;
    for10_end: skip;
  };
  for10_exit: skip;
  w16.wait?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun6(Wgdef w25; Mutexdef mu26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  if
  :: true ->
    mu26.Lock?y29;
    assert y29;
    mu26.Unlock?y29;
    assert y29;

  fi;
  defer1: skip;
  skip;
  w25.update!-(1);
  w25.update_ack?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
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