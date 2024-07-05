// git_link=loc-493-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
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
  run mutex_monitor(mu17);
  run mutex_monitor(mu16);
  run mutex_monitor(mu15);
  run fun4(mu16, mu15, child_14);
  child_14?0;
  stop_process: skip;
  child_8!0;
}
proctype fun4(Mutexdef mu18; Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  chan child_26 = [1] of {int};
  int y27 = x2;
  chan child_28 = [1] of {int};
  int y29 = x1;
  chan c30 = [0] of {int};
  for(y23 : 0 .. (y29) - (1)) {
    for10: skip;
    run fun5(mu18, mu19, c30, child_28);
    run receiver(child_28);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y29) - (1)) != (-(3))) ->
    for(y23 : 0 .. (y29) - (1)) {
      for21: skip;
      c30?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c30?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run fun6(mu18, y27, child_26);
  child_26?0;
  goto stop_process;
  stop_process: skip;
  child_20!0;
}
proctype fun5(Mutexdef mu32; Mutexdef mu33; chan c34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  c34!0;
  stop_process: skip;
  child_35!0;
}
proctype fun6(Mutexdef mu41; int y42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  int y49 = -(2);
  int y50 = -(2);
  goto stop_process;
  stop_process: skip;
  child_43!0;
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