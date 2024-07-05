// git_link=loc-326-ginger.pml
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
  chan child_15 = [1] of {int};
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
  run fun4(mu16, mu31, child_15);
  child_15?0;
  run fun4(mu16, mu31, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Mutexdef mu33; Mutexdef mu34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  chan child_41 = [1] of {int};
  Mutexdef mu42 ;
  Wgdef w43 ;
  int y44 = x2;
  int y45 = x1;
  run wg_monitor(w43);
  w43.update!y45;
  w43.update_ack?y37;
  assert y37;
  run mutex_monitor(mu42);
  for(y38 : 0 .. (y44) - (1)) {
    for10: skip;
    run fun5(w43, mu33, mu34, mu42, child_41);
    run receiver(child_41);
    for10_end: skip;
  };
  for10_exit: skip;
  w43.wait?0;
  goto stop_process;
  stop_process: skip;
  child_35!0;
}
proctype fun5(Wgdef w46; Mutexdef mu47; Mutexdef mu48; Mutexdef mu49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  chan child_56 = [1] of {int};
  run fun6(mu47, mu48, child_56);
  child_56?0;
  mu49.Lock?y52;
  assert y52;
  mu49.Unlock?y52;
  assert y52;
  w46.update!-(1);
  w46.update_ack?y52;
  assert y52;
  stop_process: skip;
  child_50!0;
}
proctype fun6(Mutexdef mu57; Mutexdef mu58; chan child_59) {
  bool y60 = false;
  bool y61 = false;
  int y62 = 0;
  bool y63 = true;
  int y64 = 0;
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
  child_59!0;
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