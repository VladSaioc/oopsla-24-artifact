// git_link=loc-568-ginger.pml
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
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
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
  run fun3(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  Mutexdef mu13 ;
  Mutexdef mu14 ;
  Wgdef w15 ;
  int y16 = x2;
  int y17 = x1;
  run wg_monitor(w15);
  chan c18 = [y17] of {int};
  run mutex_monitor(mu14);
  run mutex_monitor(mu13);
  for(y9 : 0 .. (y16) - (1)) {
    for10: skip;
    c18!0;
    w15.update!1;
    w15.update_ack?y8;
    assert y8;
    run fun4(c18, w15, mu14, mu13, child_12);
    run receiver(child_12);
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun4(chan c21; Wgdef w22; Mutexdef mu23; Mutexdef mu24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  int y31 = -(2);
  int y32 = -(2);
  c21?0;
  defer1: skip;
  skip;
  w22.update!-(1);
  w22.update_ack?y27;
  assert y27;
  stop_process: skip;
  child_25!0;
}
proctype wg_monitor(Wgdef wg) {
  int i = 0;
  end: skip;
  do
  :: wg.update?i ->
    wg.Counter = (wg.Counter) + (i);
    wg.update_ack!(wg.Counter) >= (0);
  :: (wg.Counter) == (0) ->
    end1: skip;
    if
    :: wg.update?i ->
      wg.Counter = (wg.Counter) + (i);
      wg.update_ack!(wg.Counter) >= (0);
    :: wg.wait!0 ->


    fi;

  od;
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