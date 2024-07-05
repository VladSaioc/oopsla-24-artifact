// git_link=loc-69-ginger.pml
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
  chan child_6 = [1] of {int};
  run fun3(child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun3(chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  int y16 = 0;
  Mutexdef mu17 ;
  int y18 = x2;
  int y19 = x1;
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
  run wg_monitor(w15);
  for(y10 : 0 .. (y19) - (1)) {
    for20: skip;
    for(y10 : 0 .. (y18) - (1)) {
      for21: skip;
      if
      :: true ->
        goto for21_end;
      :: true ->


      fi;
      if
      :: true ->
        goto for21_end;
      :: true ->


      fi;
      w15.update!1;
      w15.update_ack?y9;
      assert y9;
      run fun4(w15, mu17, child_14);
      run receiver(child_14);
      for21_end: skip;
    };
    for21_exit: skip;
    for20_end: skip;
  };
  for20_exit: skip;
  chan c26 = [0] of {int};
  run fun5(c26, w15, mu17, child_13);
  run receiver(child_13);
  w15.wait?0;
  run close(c26);
  stop_process: skip;
  child_7!0;
}
proctype fun4(Wgdef w27; Mutexdef mu28; chan child_29) {
  bool y30 = false;
  bool y31 = false;
  int y32 = 0;
  bool y33 = true;
  int y34 = 0;
  defer1: skip;
  skip;
  w27.update!-(1);
  w27.update_ack?y31;
  assert y31;
  stop_process: skip;
  child_29!0;
}
proctype fun5(chan c35; Wgdef w36; Mutexdef mu37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  stop_process: skip;
  child_38!0;
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