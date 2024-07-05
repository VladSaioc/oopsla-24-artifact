// git_link=loc-114-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
#define  x3 ??
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
  chan child_12 = [1] of {int};
  run fun4(child_12);
  child_12?0;
  stop_process: skip;
}
proctype fun4(chan child_13) {
  bool y14 = false;
  bool y15 = false;
  int y16 = 0;
  bool y17 = true;
  int y18 = 0;
  chan child_19 = [1] of {int};
  int y20 = x3;
  chan child_21 = [1] of {int};
  int y22 = x3;
  chan child_23 = [1] of {int};
  int y24 = x1;
  Mutexdef mu25 ;
  Mutexdef mu26 ;
  Mutexdef mu27 ;
  run mutex_monitor(mu27);
  run mutex_monitor(mu26);
  run mutex_monitor(mu25);
  run fun5(mu25, y24, child_23);
  child_23?0;
  run fun5(mu25, y22, child_21);
  child_21?0;
  run fun5(mu25, y22, child_19);
  child_19?0;
  stop_process: skip;
  child_13!0;
}
proctype fun5(Mutexdef mu28; int y29; chan child_30) {
  bool y31 = false;
  bool y32 = false;
  int y33 = 0;
  bool y34 = true;
  int y35 = 0;
  chan child_36 = [1] of {int};
  int y37 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun6(mu28, child_36);
  child_36?0;
  goto stop_process;
  stop_process: skip;
  child_30!0;
}
proctype fun6(Mutexdef mu39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  chan child_46 = [1] of {int};
  int y47 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c49 = [y47] of {int};
  chan c50 = [1] of {int};
  for(y43 : 0 .. (y47) - (1)) {
    for30: skip;
    run fun7(c49, c50, mu39, child_46);
    run receiver(child_46);
    for30_end: skip;
  };
  for30_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_40!0;
}
proctype fun7(chan c51; chan c52; Mutexdef mu53; chan child_54) {
  bool y55 = false;
  bool y56 = false;
  int y57 = 0;
  bool y58 = true;
  int y59 = 0;
  chan child_60 = [1] of {int};
  run fun8(mu53, child_60);
  child_60?0;
  if
  :: true ->
    c52!0;
  :: true ->
    c51!0;

  fi;
  stop_process: skip;
  child_54!0;
}
proctype fun8(Mutexdef mu62; chan child_63) {
  bool y64 = false;
  bool y65 = false;
  int y66 = 0;
  bool y67 = true;
  int y68 = 0;
  chan child_69 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun9(mu62, child_69);
  child_69?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_63!0;
}
proctype fun9(Mutexdef mu72; chan child_73) {
  bool y74 = false;
  bool y75 = false;
  int y76 = 0;
  bool y77 = true;
  int y78 = 0;
  chan child_79 = [1] of {int};
  chan child_80 = [1] of {int};
  int y81 = -(2);
  if
  :: ((0) != (-(2))) && (((y81) - (1)) != (-(3))) ->
    for(y76 : 0 .. (y81) - (1)) {
      for32: skip;
      run fun10(mu72, child_79);
      child_79?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for32_end: skip;
    };
    for32_exit: skip;
  :: else  ->
    do
    :: true ->
      for31: skip;
      run fun10(mu72, child_80);
      child_80?0;
      if
      :: true ->
        goto stop_process;
      :: true ->


      fi;
      for31_end: skip;
    :: true ->
      break;

    od;
    for31_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_73!0;
}
proctype fun10(Mutexdef mu84; chan child_85) {
  bool y86 = false;
  bool y87 = false;
  int y88 = 0;
  bool y89 = true;
  int y90 = 0;
  chan child_91 = [1] of {int};
  run fun11(mu84, child_91);
  child_91?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_85!0;
}
proctype fun11(Mutexdef mu93; chan child_94) {
  bool y95 = false;
  bool y96 = false;
  int y97 = 0;
  bool y98 = true;
  int y99 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_94!0;
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