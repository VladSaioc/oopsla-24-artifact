// git_link=loc-455-ginger.pml
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
  run fun2(child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun2(chan child_5) {
  bool y6 = false;
  bool y7 = false;
  int y8 = 0;
  bool y9 = true;
  int y10 = 0;
  chan child_11 = [1] of {int};
  Mutexdef mu12 ;
  int y13 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  if
  :: true ->
    run mutex_monitor(mu12);
  :: true ->


  fi;
  if
  :: true ->
    chan c19 = [y13] of {int};
    for(y8 : 0 .. (y13) - (1)) {
      for10: skip;
      run fun3(c19, mu12, child_11);
      run receiver(child_11);
      for10_end: skip;
    };
    for10_exit: skip;
    if
    :: ((0) != (-(2))) && (((y13) - (1)) != (-(3))) ->
      for(y8 : 0 .. (y13) - (1)) {
        for21: skip;
        c19?0;
        for21_end: skip;
      };
      for21_exit: skip;
    :: else  ->
      do
      :: true ->
        for20: skip;
        c19?0;
        for20_end: skip;
      :: true ->
        break;

      od;
      for20_exit: skip;
    fi;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_5!0;
}
proctype fun3(chan c22; Mutexdef mu23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  c22!0;
  stop_process: skip;
  child_24!0;
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