// git_link=loc-462-ginger.pml
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
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Mutexdef mu15 ;
  Mutexdef mu16 ;
  run mutex_monitor(mu16);
  run mutex_monitor(mu15);
  chan c17 = [0] of {int};
  chan c18 = [0] of {int};
  chan c19 = [0] of {int};
  for(y10 : 0 .. (y6) - (1)) {
    for10: skip;
    run fun4(c17, c18, c19, child_14);
    run receiver(child_14);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y6) - (1)) {
      for21: skip;
      c17?0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c17?0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y6) - (1)) {
      for31: skip;
      c18!0;
      for31_end: skip;
    };
    for31_exit: skip;
  :: else  ->
    do
    :: true ->
      for30: skip;
      c18!0;
      for30_end: skip;
    :: true ->
      break;

    od;
    for30_exit: skip;
  fi;
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y10 : 0 .. (y6) - (1)) {
      for41: skip;
      c19?0;
      for41_end: skip;
    };
    for41_exit: skip;
  :: else  ->
    do
    :: true ->
      for40: skip;
      c19?0;
      for40_end: skip;
    :: true ->
      break;

    od;
    for40_exit: skip;
  fi;
  defer1: skip;
  skip;
  run fun3(mu16, mu15, child_13);
  child_13?0;
  stop_process: skip;
  child_7!0;
}
proctype fun3(Mutexdef mu23; Mutexdef mu24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  stop_process: skip;
  child_25!0;
}
proctype fun4(chan c31; chan c32; chan c33; chan child_34) {
  bool y35 = false;
  bool y36 = false;
  int y37 = 0;
  bool y38 = true;
  int y39 = 0;
  c31!0;
  c32?0;
  c33!0;
  stop_process: skip;
  child_34!0;
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