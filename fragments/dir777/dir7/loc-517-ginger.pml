// git_link=loc-517-ginger.pml
#define  default true
#define  x1 ??
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
}
init {
  chan child_8 = [1] of {int};
  run fun2(child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun2(chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  chan child_18 = [1] of {int};
  chan child_19 = [1] of {int};
  chan child_20 = [1] of {int};
  chan child_21 = [1] of {int};
  chan child_22 = [1] of {int};
  Mutexdef mu23 ;
  run mutex_monitor(mu23);
  run fun3(mu23, child_22);
  child_22?0;
  run fun3(mu23, child_21);
  child_21?0;
  run fun5(mu23, child_20);
  child_20?0;
  run fun6(mu23, child_19);
  child_19?0;
  run fun6(mu23, child_18);
  child_18?0;
  run fun5(mu23, child_17);
  child_17?0;
  run fun7(mu23, child_16);
  child_16?0;
  run fun5(mu23, child_15);
  child_15?0;
  stop_process: skip;
  child_9!0;
}
proctype fun3(Mutexdef mu24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  chan child_31 = [1] of {int};
  run fun4(mu24, child_31);
  child_31?0;
  mu24.Lock?y27;
  assert y27;
  defer1: skip;
  skip;
  mu24.Unlock?y27;
  assert y27;
  stop_process: skip;
  child_25!0;
}
proctype fun4(Mutexdef mu32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  mu32.Lock?y35;
  assert y35;
  defer1: skip;
  skip;
  mu32.Unlock?y35;
  assert y35;
  stop_process: skip;
  child_33!0;
}
proctype fun5(Mutexdef mu39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  chan child_46 = [1] of {int};
  run fun4(mu39, child_46);
  child_46?0;
  mu39.Lock?y42;
  assert y42;
  if
  :: ((0) != (-(2))) && (((x1) - (1)) != (-(3))) ->
    for(y43 : 0 .. (x1) - (1)) {
      for11: skip;
      mu39.Unlock?y42;
      assert y42;
      mu39.Lock?y42;
      assert y42;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      mu39.Unlock?y42;
      assert y42;
      mu39.Lock?y42;
      assert y42;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  defer1: skip;
  skip;
  mu39.Unlock?y42;
  assert y42;
  stop_process: skip;
  child_40!0;
}
proctype fun6(Mutexdef mu48; chan child_49) {
  bool y50 = false;
  bool y51 = false;
  int y52 = 0;
  bool y53 = true;
  int y54 = 0;
  chan child_55 = [1] of {int};
  run fun4(mu48, child_55);
  child_55?0;
  mu48.Lock?y51;
  assert y51;
  defer1: skip;
  skip;
  mu48.Unlock?y51;
  assert y51;
  stop_process: skip;
  child_49!0;
}
proctype fun7(Mutexdef mu56; chan child_57) {
  bool y58 = false;
  bool y59 = false;
  int y60 = 0;
  bool y61 = true;
  int y62 = 0;
  chan child_63 = [1] of {int};
  run fun4(mu56, child_63);
  child_63?0;
  mu56.Lock?y59;
  assert y59;
  defer1: skip;
  skip;
  mu56.Unlock?y59;
  assert y59;
  stop_process: skip;
  child_57!0;
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