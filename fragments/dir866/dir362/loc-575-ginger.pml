// git_link=loc-575-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun2(x1, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Mutexdef mu13 ;
  run mutex_monitor(mu13);
  chan c14 = [y5] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  for(y9 : 0 .. (y5) - (1)) {
    for10: skip;
    run fun3(c14, mu13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  if
  :: ((y5) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y5) - (1)) {
      for20: skip;
      c14?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      c14?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for21_end: skip;
    :: true ->
      break;

    od;
    for21_exit: skip;
  fi;
  run close(c14);
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c18; Mutexdef mu19; chan child_20) {
  bool y21 = false;
  bool y22 = false;
  int y23 = 0;
  bool y24 = true;
  int y25 = 0;
  int y26 = -(2);
  mu19.Lock?y22;
  assert y22;
  if
  :: true ->
    c18!0;
    mu19.Unlock?y22;
    assert y22;
    goto stop_process;
  :: true ->


  fi;
  c18!0;
  mu19.Unlock?y22;
  assert y22;
  stop_process: skip;
  child_20!0;
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