// git_link=loc-38-ginger.pml
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
  chan child_4 = [1] of {int};
  run fun3(x2, child_4);
  child_4?0;
  stop_process: skip;
}
proctype fun3(int y5; chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  Mutexdef mu12 ;
  int y13 = x1;
  run mutex_monitor(mu12);
  chan c14 = [y13] of {int};
  if
  :: ((y5) - (1)) != (-(3)) ->
    for(y9 : 0 .. (y5) - (1)) {
      for10: skip;
      if
      :: true ->
        c14!0;
        if
        :: true ->
          mu12.Lock?y8;
          assert y8;
          mu12.Unlock?y8;
          assert y8;
        :: true ->


        fi;
        c14?0;
      :: true ->


      fi;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for11: skip;
      if
      :: true ->
        c14!0;
        if
        :: true ->
          mu12.Lock?y8;
          assert y8;
          mu12.Unlock?y8;
          assert y8;
        :: true ->


        fi;
        c14?0;
      :: true ->


      fi;
      for11_end: skip;
    :: true ->
      break;

    od;
    for11_exit: skip;
  fi;
  if
  :: ((0) != (-(2))) && (((y13) - (1)) != (-(3))) ->
    for(y9 : 0 .. (y13) - (1)) {
      for21: skip;
      c14!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c14!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  run close(c14);
  goto stop_process;
  stop_process: skip;
  child_6!0;
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