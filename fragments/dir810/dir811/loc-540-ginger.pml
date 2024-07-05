// git_link=loc-540-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun2(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun2(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  Wgdef w16 ;
  Mutexdef mu17 ;
  Mutexdef mu18 ;
  int y19 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run mutex_monitor(mu18);
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
  run mutex_monitor(mu17);
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
    goto stop_process;
  :: true ->


  fi;
  chan c26 = [0] of {int};
  run wg_monitor(w16);
  w16.update!2;
  w16.update_ack?y9;
  assert y9;
  run fun3(c26, w16, child_15);
  run receiver(child_15);
  run fun3(c26, w16, child_14);
  run receiver(child_14);
  chan c27 = [0] of {int};
  run fun4(mu17, c27, w16, child_13);
  run receiver(child_13);
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c28; Wgdef w29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  if
  :: ((0) != (-(2))) && (((x1) - (1)) != (-(3))) ->
    for(y33 : 0 .. (x1) - (1)) {
      for21: skip;
      c28!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      c28!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  w29.update!-(1);
  w29.update_ack?y32;
  assert y32;
  stop_process: skip;
  child_30!0;
}
proctype fun4(Mutexdef mu37; chan c38; Wgdef w39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  chan child_45 = [1] of {int};
  int y46 = 0;
  if
  :: true ->
    if
    :: true ->
      if
      :: true ->
        w39.wait?0;
        run fun5(c38, child_45);
        c38!0;
        goto stop_process;
      :: true ->


      fi;
    :: true ->


    fi;
    c38!0;
    goto stop_process;
  :: true ->


  fi;
  c38!0;
  stop_process: skip;
  child_40!0;
}
proctype fun5(chan c50; chan child_51) {
  c50?0;
  stop_process: skip;
  child_51!0;
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