// git_link=loc-516-ginger.pml
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
  chan child_23 = [1] of {int};
  chan child_24 = [1] of {int};
  Mutexdef mu25 ;
  run mutex_monitor(mu25);
  run fun3(mu25, child_24);
  child_24?0;
  run fun3(mu25, child_23);
  child_23?0;
  run fun5(mu25, child_22);
  child_22?0;
  run fun5(mu25, child_21);
  child_21?0;
  run fun3(mu25, child_20);
  child_20?0;
  run fun6(mu25, child_19);
  child_19?0;
  run fun5(mu25, child_18);
  child_18?0;
  run fun6(mu25, child_17);
  child_17?0;
  run fun7(mu25, child_16);
  child_16?0;
  run fun3(mu25, child_15);
  child_15?0;
  stop_process: skip;
  child_9!0;
}
proctype fun3(Mutexdef mu26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  run fun4(mu26, child_33);
  child_33?0;
  mu26.Lock?y29;
  assert y29;
  defer1: skip;
  skip;
  mu26.Unlock?y29;
  assert y29;
  stop_process: skip;
  child_27!0;
}
proctype fun4(Mutexdef mu34; chan child_35) {
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
proctype fun5(Mutexdef mu41; chan child_42) {
  bool y43 = false;
  bool y44 = false;
  int y45 = 0;
  bool y46 = true;
  int y47 = 0;
  chan child_48 = [1] of {int};
  run fun4(mu41, child_48);
  child_48?0;
  mu41.Lock?y44;
  assert y44;
  defer1: skip;
  skip;
  mu41.Unlock?y44;
  assert y44;
  stop_process: skip;
  child_42!0;
}
proctype fun6(Mutexdef mu49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  chan child_56 = [1] of {int};
  run fun4(mu49, child_56);
  child_56?0;
  mu49.Lock?y52;
  assert y52;
  if
  :: ((0) != (-(2))) && (((x1) - (1)) != (-(3))) ->
    for(y53 : 0 .. (x1) - (1)) {
      for11: skip;
      mu49.Unlock?y52;
      assert y52;
      mu49.Lock?y52;
      assert y52;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      mu49.Unlock?y52;
      assert y52;
      mu49.Lock?y52;
      assert y52;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  defer1: skip;
  skip;
  mu49.Unlock?y52;
  assert y52;
  stop_process: skip;
  child_50!0;
}
proctype fun7(Mutexdef mu58; chan child_59) {
  bool y60 = false;
  bool y61 = false;
  int y62 = 0;
  bool y63 = true;
  int y64 = 0;
  chan child_65 = [1] of {int};
  run fun4(mu58, child_65);
  child_65?0;
  mu58.Lock?y61;
  assert y61;
  defer1: skip;
  skip;
  mu58.Unlock?y61;
  assert y61;
  stop_process: skip;
  child_59!0;
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