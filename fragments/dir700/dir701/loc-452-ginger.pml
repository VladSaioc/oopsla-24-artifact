// git_link=loc-452-ginger.pml
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
  chan child_3 = [1] of {int};
  run fun2(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun2(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  int y10 = 0;
  int y11 = 0;
  Mutexdef mu12 ;
  Wgdef w13 ;
  Mutexdef mu14 ;
  Mutexdef mu15 ;
  int y16 = -(2);
  int y17 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  run mutex_monitor(mu15);
  run mutex_monitor(mu14);
  run wg_monitor(w13);
  run mutex_monitor(mu12);
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    if
    :: true ->
      goto stop_process;

    fi;

  fi;
  if
  :: true ->
    if
    :: true ->
      assert (20) == (0);
    :: true ->


    fi;
    if
    :: true ->
      assert (20) == (0);
    :: true ->


    fi;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->
    if
    :: true ->
      assert (20) == (0);
    :: true ->


    fi;
    if
    :: true ->
      if
      :: true ->
        assert (20) == (0);

      fi;

    fi;

  fi;
  chan c38 = [x1] of {int};
  chan c39 = [y10] of {int};
  if
  :: ((y10) - (1)) != (-(3)) ->
    for(y7 : 0 .. (y10) - (1)) {
      for40: skip;
      c38!0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for40_end: skip;
    };
    for40_exit: skip;
  :: else  ->
    do
    :: true ->
      for41: skip;
      c38!0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for41_end: skip;
    :: true ->
      break;

    od;
    for41_exit: skip;
  fi;
  run close(c38);
  if
  :: ((0) != (-(2))) && (((y10) - (1)) != (-(3))) ->
    for(y7 : 0 .. (y10) - (1)) {
      for51: skip;
      c39?0;
      if
      :: true ->
        assert (20) == (0);
      :: true ->


      fi;
      for51_end: skip;
    };
    for51_exit: skip;
  :: else  ->
    do
    :: true ->
      for50: skip;
      c39?0;
      if
      :: true ->
        assert (20) == (0);
      :: true ->


      fi;
      for50_end: skip;
    :: true ->
      break;

    od;
    for50_exit: skip;
  fi;
  run close(c39);
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  if
  :: true ->
    assert (20) == (0);
  :: true ->


  fi;
  stop_process: skip;
  child_4!0;
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