// git_link=loc-328-ginger.pml
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
  Mutexdef mu15 ;
  Mutexdef mu16 ;
  Mutexdef mu17 ;
  Mutexdef mu18 ;
  Mutexdef mu19 ;
  Mutexdef mu20 ;
  Mutexdef mu21 ;
  Mutexdef mu22 ;
  Mutexdef mu23 ;
  Mutexdef mu24 ;
  Mutexdef mu25 ;
  Mutexdef mu26 ;
  Mutexdef mu27 ;
  Mutexdef mu28 ;
  Mutexdef mu29 ;
  Mutexdef mu30 ;
  Mutexdef mu31 ;
  Mutexdef mu32 ;
  int y33 = -(2);
  run mutex_monitor(mu32);
  run mutex_monitor(mu31);
  run mutex_monitor(mu30);
  run mutex_monitor(mu29);
  run mutex_monitor(mu28);
  run mutex_monitor(mu27);
  run mutex_monitor(mu26);
  run mutex_monitor(mu25);
  run mutex_monitor(mu24);
  run mutex_monitor(mu23);
  run mutex_monitor(mu22);
  run mutex_monitor(mu21);
  run mutex_monitor(mu20);
  run mutex_monitor(mu19);
  run mutex_monitor(mu18);
  run mutex_monitor(mu17);
  run mutex_monitor(mu16);
  run mutex_monitor(mu15);
  run fun4(mu15, mu31, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Mutexdef mu34; Mutexdef mu35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  chan child_42 = [1] of {int};
  Mutexdef mu43 ;
  Wgdef w44 ;
  int y45 = x2;
  int y46 = x1;
  run wg_monitor(w44);
  w44.update!y46;
  w44.update_ack?y38;
  assert y38;
  run mutex_monitor(mu43);
  for(y39 : 0 .. (y45) - (1)) {
    for20: skip;
    run fun5(w44, mu34, mu35, mu43, child_42);
    run receiver(child_42);
    for20_end: skip;
  };
  for20_exit: skip;
  w44.wait?0;
  goto stop_process;
  stop_process: skip;
  child_36!0;
}
proctype fun5(Wgdef w47; Mutexdef mu48; Mutexdef mu49; Mutexdef mu50; chan child_51) {
  bool y52 = false;
  bool y53 = false;
  int y54 = 0;
  bool y55 = true;
  int y56 = 0;
  chan child_57 = [1] of {int};
  run fun6(mu48, mu49, child_57);
  child_57?0;
  mu50.Lock?y53;
  assert y53;
  mu50.Unlock?y53;
  assert y53;
  w47.update!-(1);
  w47.update_ack?y53;
  assert y53;
  stop_process: skip;
  child_51!0;
}
proctype fun6(Mutexdef mu58; Mutexdef mu59; chan child_60) {
  bool y61 = false;
  bool y62 = false;
  int y63 = 0;
  bool y64 = true;
  int y65 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->
    goto stop_process;
  :: true ->
    goto stop_process;
  :: true ->
    goto stop_process;
  :: true ->
    goto stop_process;

  fi;
  stop_process: skip;
  child_60!0;
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