// git_link=loc-316-ginger.pml
#define  default true
#define  x1 ??
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
  chan child_5 = [1] of {int};
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  int y13 = x1;
  Mutexdef mu14 ;
  Mutexdef mu15 ;
  run mutex_monitor(mu15);
  run mutex_monitor(mu14);
  chan c16 = [0] of {int};
  for(y9 : 0 .. (y13) - (1)) {
    for10: skip;
    run fun3(c16, mu15, mu14, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y13) - (1)) != (-(3))) ->
    for(y9 : 0 .. (y13) - (1)) {
      for21: skip;
      c16?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c16?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c18; Mutexdef mu19; Mutexdef mu20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  chan child_27 = [1] of {int};
  run fun4(mu19, mu20, c18, child_27);
  child_27?0;
  stop_process: skip;
  child_21!0;
}
proctype fun4(Mutexdef mu28; Mutexdef mu29; chan c30; chan child_31) {
  bool y32 = false;
  bool y33 = false;
  int y34 = 0;
  bool y35 = true;
  int y36 = 0;
  c30!0;
  stop_process: skip;
  child_31!0;
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
