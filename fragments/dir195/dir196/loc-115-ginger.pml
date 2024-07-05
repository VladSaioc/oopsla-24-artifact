// git_link=loc-115-ginger.pml
#define  default true
typedef Chandef {
  chan sync = [0] of {bool};
  chan enq = [0] of {bool};
  chan deq = [0] of {bool,bool};
  chan sending = [0] of {bool};
  chan rcving = [0] of {bool};
  chan closing = [0] of {bool};
  int size = 0;
  int num_msgs = 0;
  bool closed = false;
}
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
}
init {
  chan child_9 = [1] of {int};
  run fun1(child_9);
  child_9?0;
  stop_process: skip;
}
proctype fun1(chan child_10) {
  bool y11 = false;
  bool y12 = false;
  int y13 = 0;
  bool y14 = true;
  int y15 = 0;
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  Mutexdef mu18 ;
  run mutex_monitor(mu18);
  run fun2(mu18, child_17);
  child_17?0;
  run fun2(mu18, child_16);
  child_16?0;
  stop_process: skip;
  child_10!0;
}
proctype fun2(Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  chan child_26 = [1] of {int};
  chan child_27 = [1] of {int};
  int y28 = 1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c30 = [y28] of {int};
  run fun3(c30, mu19, child_27);
  run receiver(child_27);
  if
  :: (y28) > (1) ->
    run fun7(c30, mu19, child_26);
    run receiver(child_26);
  :: else  ->
  fi;
  goto stop_process;
  stop_process: skip;
  child_20!0;
}
proctype fun3(chan c32; Mutexdef mu33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  chan child_40 = [1] of {int};
  run fun4(mu33, child_40);
  child_40?0;
  c32!0;
  stop_process: skip;
  child_34!0;
}
proctype fun4(Mutexdef mu41; chan child_42) {
  bool y43 = false;
  bool y44 = false;
  int y45 = 0;
  bool y46 = true;
  int y47 = 0;
  chan child_48 = [1] of {int};
  chan child_49 = [1] of {int};
  int y50 = -(2);
  if
  :: ((0) != (-(2))) && (((y50) - (1)) != (-(3))) ->
    for(y45 : 0 .. (y50) - (1)) {
      for11: skip;
      run fun5(mu41, child_48);
      child_48?0;
      if
      :: true ->
        break;
      :: true ->


      fi;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      run fun5(mu41, child_49);
      child_49?0;
      if
      :: true ->
        break;
      :: true ->


      fi;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_42!0;
}
proctype fun5(Mutexdef mu53; chan child_54) {
  bool y55 = false;
  bool y56 = false;
  int y57 = 0;
  bool y58 = true;
  int y59 = 0;
  chan child_60 = [1] of {int};
  run fun6(mu53, child_60);
  child_60?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_54!0;
}
proctype fun6(Mutexdef mu62; chan child_63) {
  bool y64 = false;
  bool y65 = false;
  int y66 = 0;
  bool y67 = true;
  int y68 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_63!0;
}
proctype fun7(chan c70; Mutexdef mu71; chan child_72) {
  bool y73 = false;
  bool y74 = false;
  int y75 = 0;
  bool y76 = true;
  int y77 = 0;
  chan child_78 = [1] of {int};
  run fun8(mu71, child_78);
  child_78?0;
  c70!0;
  stop_process: skip;
  child_72!0;
}
proctype fun8(Mutexdef mu79; chan child_80) {
  bool y81 = false;
  bool y82 = false;
  int y83 = 0;
  bool y84 = true;
  int y85 = 0;
  chan child_86 = [1] of {int};
  run fun4(mu79, child_86);
  child_86?0;
  goto stop_process;
  stop_process: skip;
  child_80!0;
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