// git_link=loc-463-ginger.pml
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
  run fun3(x1, x2, child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun3(int y8; int y9; chan child_10) {
  bool y11 = false;
  bool y12 = false;
  int y13 = 0;
  bool y14 = true;
  int y15 = 0;
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  chan child_18 = [1] of {int};
  chan child_19 = [1] of {int};
  chan child_20 = [1] of {int};
  Mutexdef mu21 ;
  Mutexdef mu22 ;
  run mutex_monitor(mu22);
  run mutex_monitor(mu21);
  chan c23 = [0] of {int};
  run fun5(c23, y8, child_20);
  run receiver(child_20);
  for(y13 : 0 .. ((y9) / (2)) - (1)) {
    for20: skip;
    run fun6(c23, y8, child_19);
    run receiver(child_19);
    for20_end: skip;
  };
  for20_exit: skip;
  run fun5(c23, y8, child_18);
  run receiver(child_18);
  for(y13 : (y9) / (2) .. (y9) - (1)) {
    for30: skip;
    run fun6(c23, y8, child_17);
    run receiver(child_17);
    for30_end: skip;
  };
  for30_exit: skip;
  if
  :: ((0) != (-(2))) && ((((2) + (y9)) - (1)) != (-(3))) ->
    for(y13 : 0 .. ((2) + (y9)) - (1)) {
      for41: skip;
      c23?0;
      for41_end: skip;
    };
    for41_exit: skip;
  :: else  ->
    do
    :: true ->
      for40: skip;
      c23?0;
      for40_end: skip;
    :: true ->
      break;

    od;
    for40_exit: skip;
  fi;
  defer1: skip;
  skip;
  run fun4(mu22, mu21, child_16);
  child_16?0;
  stop_process: skip;
  child_10!0;
}
proctype fun4(Mutexdef mu25; Mutexdef mu26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  stop_process: skip;
  child_27!0;
}
proctype fun5(chan c33; int y34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  c33!0;
  stop_process: skip;
  child_35!0;
}
proctype fun6(chan c41; int y42; chan child_43) {
  bool y44 = false;
  bool y45 = false;
  int y46 = 0;
  bool y47 = true;
  int y48 = 0;
  c41!0;
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