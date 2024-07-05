// git_link=loc-113-ginger.pml
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
  chan child_10 = [1] of {int};
  run fun3(child_10);
  child_10?0;
  stop_process: skip;
}
proctype fun3(chan child_11) {
  bool y12 = false;
  bool y13 = false;
  int y14 = 0;
  bool y15 = true;
  int y16 = 0;
  chan child_17 = [1] of {int};
  int y18 = x2;
  chan child_19 = [1] of {int};
  int y20 = x2;
  chan child_21 = [1] of {int};
  int y22 = x1;
  Mutexdef mu23 ;
  Mutexdef mu24 ;
  Mutexdef mu25 ;
  run mutex_monitor(mu25);
  run mutex_monitor(mu24);
  run mutex_monitor(mu23);
  run fun4(mu23, y22, child_21);
  child_21?0;
  run fun4(mu23, y20, child_19);
  child_19?0;
  run fun4(mu23, y20, child_17);
  child_17?0;
  stop_process: skip;
  child_11!0;
}
proctype fun4(Mutexdef mu26; int y27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  chan child_34 = [1] of {int};
  int y35 = y27;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c37 = [y35] of {int};
  for(y31 : 0 .. (y35) - (1)) {
    for20: skip;
    run fun5(c37, mu26, child_34);
    run receiver(child_34);
    for20_end: skip;
  };
  for20_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_28!0;
}
proctype fun5(chan c38; Mutexdef mu39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  chan child_46 = [1] of {int};
  run fun6(mu39, child_46);
  child_46?0;
  c38!0;
  stop_process: skip;
  child_40!0;
}
proctype fun6(Mutexdef mu47; chan child_48) {
  bool y49 = false;
  bool y50 = false;
  int y51 = 0;
  bool y52 = true;
  int y53 = 0;
  chan child_54 = [1] of {int};
  run fun7(mu47, child_54);
  child_54?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_48!0;
}
proctype fun7(Mutexdef mu56; chan child_57) {
  bool y58 = false;
  bool y59 = false;
  int y60 = 0;
  bool y61 = true;
  int y62 = 0;
  chan child_63 = [1] of {int};
  chan child_64 = [1] of {int};
  int y65 = -(2);
  if
  :: ((0) != (-(2))) && (((y65) - (1)) != (-(3))) ->
    for(y60 : 0 .. (y65) - (1)) {
      for22: skip;
      run fun8(mu56, child_63);
      child_63?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for22_end: skip;
    };
    for22_exit: skip;
  :: else  ->
    do
    :: true ->
      for21: skip;
      run fun8(mu56, child_64);
      child_64?0;
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
  goto stop_process;
  stop_process: skip;
  child_57!0;
}
proctype fun8(Mutexdef mu68; chan child_69) {
  bool y70 = false;
  bool y71 = false;
  int y72 = 0;
  bool y73 = true;
  int y74 = 0;
  chan child_75 = [1] of {int};
  run fun9(mu68, child_75);
  child_75?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_69!0;
}
proctype fun9(Mutexdef mu77; chan child_78) {
  bool y79 = false;
  bool y80 = false;
  int y81 = 0;
  bool y82 = true;
  int y83 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_78!0;
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