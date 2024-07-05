// git_link=loc-327-ginger.pml
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
  run fun4(mu15, mu30, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Mutexdef mu32; Mutexdef mu33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  chan child_40 = [1] of {int};
  Mutexdef mu41 ;
  Wgdef w42 ;
  int y43 = x2;
  int y44 = x1;
  run wg_monitor(w42);
  w42.update!y44;
  w42.update_ack?y36;
  assert y36;
  run mutex_monitor(mu41);
  for(y37 : 0 .. (y43) - (1)) {
    for10: skip;
    run fun5(w42, mu32, mu33, mu41, child_40);
    run receiver(child_40);
    for10_end: skip;
  };
  for10_exit: skip;
  w42.wait?0;
  goto stop_process;
  stop_process: skip;
  child_34!0;
}
proctype fun5(Wgdef w45; Mutexdef mu46; Mutexdef mu47; Mutexdef mu48; chan child_49) {
  bool y50 = false;
  bool y51 = false;
  int y52 = 0;
  bool y53 = true;
  int y54 = 0;
  chan child_55 = [1] of {int};
  run fun6(mu46, mu47, child_55);
  child_55?0;
  mu48.Lock?y51;
  assert y51;
  mu48.Unlock?y51;
  assert y51;
  w45.update!-(1);
  w45.update_ack?y51;
  assert y51;
  stop_process: skip;
  child_49!0;
}
proctype fun6(Mutexdef mu56; Mutexdef mu57; chan child_58) {
  bool y59 = false;
  bool y60 = false;
  int y61 = 0;
  bool y62 = true;
  int y63 = 0;
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
  child_58!0;
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