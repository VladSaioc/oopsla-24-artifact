// git_link=loc-514-ginger.pml
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
  chan child_7 = [1] of {int};
  run fun2(child_7);
  child_7?0;
  stop_process: skip;
}
proctype fun2(chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  chan child_18 = [1] of {int};
  Mutexdef mu19 ;
  Wgdef w20 ;
  int y21 = x1;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run wg_monitor(w20);
  run mutex_monitor(mu19);
  for(y11 : 0 .. (y21) - (1)) {
    for10: skip;
    if
    :: true ->
      assert (20) == (0);
    :: true ->


    fi;
    if
    :: true ->
      run fun3(mu19, child_15);
      child_15?0;
    :: true ->
      run fun4(mu19, child_16);
      child_16?0;
    :: true ->
      if
      :: true ->
        run fun3(mu19, child_17);
        child_17?0;
      :: true ->
        run fun4(mu19, child_18);
        child_18?0;

      fi;
    :: true ->

    :: true ->

    :: true ->

    :: true ->
      assert (20) == (0);

    fi;
    run fun5(w20, child_14);
    child_14?0;
    for10_end: skip;
  };
  for10_exit: skip;
  w20.wait?0;
  stop_process: skip;
  child_8!0;
}
proctype fun3(Mutexdef mu22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  goto stop_process;
  stop_process: skip;
  child_31!0;
}
proctype fun4(Mutexdef mu29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  int y36 = -(2);
  goto stop_process;
  stop_process: skip;
  child_30!0;
}
proctype fun5(Wgdef w37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  chan child_44 = [1] of {int};
  w37.update!1;
  w37.update_ack?y40;
  assert y40;
  run fun6(w37, child_44);
  run receiver(child_44);
  stop_process: skip;
  child_38!0;
}
proctype fun6(Wgdef w45; chan child_46) {
  bool y47 = false;
  bool y48 = false;
  int y49 = 0;
  bool y50 = true;
  int y51 = 0;
  defer1: skip;
  skip;
  w45.update!-(1);
  w45.update_ack?y48;
  assert y48;
  stop_process: skip;
  child_46!0;
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
