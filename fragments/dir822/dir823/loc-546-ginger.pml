// git_link=loc-546-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun2(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun2(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  Mutexdef mu18 ;
  Wgdef w19 ;
  int y20 = x1;
  Wgdef w21 ;
  run wg_monitor(w21);
  run wg_monitor(w19);
  run mutex_monitor(mu18);
  run fun3(w19, mu18, child_17);
  child_17?0;
  for(y11 : 0 .. (y20) - (1)) {
    for10: skip;
    w21.update!1;
    w21.update_ack?y10;
    assert y10;
    run fun4(w21, w19, mu18, child_16);
    run receiver(child_16);
    w21.update!1;
    w21.update_ack?y10;
    assert y10;
    run fun5(w21, w19, mu18, child_15);
    run receiver(child_15);
    for10_end: skip;
  };
  for10_exit: skip;
  w21.wait?0;
  run fun6(w19, mu18, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun3(Wgdef w22; Mutexdef mu23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  stop_process: skip;
  child_24!0;
}
proctype fun4(Wgdef w30; Wgdef w31; Mutexdef mu32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  chan child_39 = [1] of {int};
  run fun3(w31, mu32, child_39);
  child_39?0;
  defer1: skip;
  skip;
  w30.update!-(1);
  w30.update_ack?y35;
  assert y35;
  stop_process: skip;
  child_33!0;
}
proctype fun5(Wgdef w40; Wgdef w41; Mutexdef mu42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  chan child_49 = [1] of {int};
  run fun6(w41, mu42, child_49);
  child_49?0;
  defer1: skip;
  skip;
  w40.update!-(1);
  w40.update_ack?y45;
  assert y45;
  stop_process: skip;
  child_43!0;
}
proctype fun6(Wgdef w50; Mutexdef mu51; chan child_52) {
  bool y53 = false;
  bool y54 = false;
  int y55 = 0;
  bool y56 = true;
  int y57 = 0;
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
