// git_link=loc-473-ginger.pml
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
  chan child_15 = [1] of {int};
  run fun4(child_15);
  child_15?0;
  stop_process: skip;
}
proctype fun4(chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  chan child_22 = [1] of {int};
  Mutexdef mu23 ;
  Mutexdef mu24 ;
  Mutexdef mu25 ;
  int y26 = -(2);
  run mutex_monitor(mu25);
  run mutex_monitor(mu24);
  run mutex_monitor(mu23);
  run fun5(mu24, child_22);
  child_22?0;
  stop_process: skip;
  child_16!0;
}
proctype fun5(Mutexdef mu27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  chan child_34 = [1] of {int};
  int y35 = x1;
  run fun6(mu27, y35, child_34);
  child_34?0;
  goto stop_process;
  stop_process: skip;
  child_28!0;
}
proctype fun6(Mutexdef mu36; int y37; chan child_38) {
  bool y39 = false;
  bool y40 = false;
  int y41 = 0;
  bool y42 = true;
  int y43 = 0;
  chan child_44 = [1] of {int};
  chan child_45 = [1] of {int};
  chan child_46 = [1] of {int};
  chan child_47 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun7(mu36, child_47);
  child_47?0;
  if
  :: ((y37) - (1)) != (-(3)) ->
    for(y41 : 0 .. (y37) - (1)) {
      for30: skip;
      run fun8(mu36, child_45);
      child_45?0;
      for30_end: skip;
    };
    for30_exit: skip;
  :: else  ->
    do
    :: true ->
      for31: skip;
      run fun8(mu36, child_46);
      child_46?0;
      for31_end: skip;
    :: true ->
      break;

    od;
    for31_exit: skip;
  fi;
  run fun9(mu36, x2, child_44);
  child_44?0;
  goto stop_process;
  stop_process: skip;
  child_38!0;
}
proctype fun7(Mutexdef mu50; chan child_51) {
  bool y52 = false;
  bool y53 = false;
  int y54 = 0;
  bool y55 = true;
  int y56 = 0;
  int y57 = -(2);
  goto stop_process;
  stop_process: skip;
  child_51!0;
}
proctype fun8(Mutexdef mu58; chan child_59) {
  bool y60 = false;
  bool y61 = false;
  int y62 = 0;
  bool y63 = true;
  int y64 = 0;
  goto stop_process;
  stop_process: skip;
  child_59!0;
}
proctype fun9(Mutexdef mu65; int y66; chan child_67) {
  bool y68 = false;
  bool y69 = false;
  int y70 = 0;
  bool y71 = true;
  int y72 = 0;
  chan child_73 = [1] of {int};
  int y74 = x3;
  chan c75 = [y66] of {int};
  for(y70 : 0 .. (y66) - (1)) {
    for40: skip;
    run fun10(c75, mu65, child_73);
    run receiver(child_73);
    for40_end: skip;
  };
  for40_exit: skip;
  if
  :: ((0) != (-(2))) && (((y66) - (1)) != (-(3))) ->
    for(y70 : 0 .. (y66) - (1)) {
      for53: skip;
      c75?0;
      if
      :: true ->
        goto for53_end;
      :: true ->


      fi;
      if
      :: true ->
        goto for53_end;
      :: true ->


      fi;
      for53_end: skip;
    };
    for53_exit: skip;
  :: else  ->
    do
    :: true ->
      for50: skip;
      c75?0;
      if
      :: true ->
        goto for50_end;
      :: true ->


      fi;
      if
      :: true ->
        goto for50_end;
      :: true ->


      fi;
      for50_end: skip;
    :: true ->
      break;

    od;
    for50_exit: skip;
  fi;
  goto stop_process;
  stop_process: skip;
  child_67!0;
}
proctype fun10(chan c79; Mutexdef mu80; chan child_81) {
  bool y82 = false;
  bool y83 = false;
  int y84 = 0;
  bool y85 = true;
  int y86 = 0;
  chan child_87 = [1] of {int};
  run fun11(mu80, child_87);
  child_87?0;
  c79!0;
  stop_process: skip;
  child_81!0;
}
proctype fun11(Mutexdef mu88; chan child_89) {
  bool y90 = false;
  bool y91 = false;
  int y92 = 0;
  bool y93 = true;
  int y94 = 0;
  chan child_95 = [1] of {int};
  chan child_96 = [1] of {int};
  chan child_97 = [1] of {int};
  chan child_98 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  run fun12(mu88, child_98);
  child_98?0;
  if
  :: true ->
    run fun13(mu88, child_97);
    child_97?0;
  :: true ->


  fi;
  run fun12(mu88, child_96);
  child_96?0;
  if
  :: true ->
    run fun14(mu88, child_95);
    child_95?0;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
  :: true ->
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;

  fi;
  goto stop_process;
  stop_process: skip;
  child_89!0;
}
proctype fun12(Mutexdef mu104; chan child_105) {
  bool y106 = false;
  bool y107 = false;
  int y108 = 0;
  bool y109 = true;
  int y110 = 0;
  int y111 = -(2);
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
  goto stop_process;
  stop_process: skip;
  child_105!0;
}
proctype fun13(Mutexdef mu114; chan child_115) {
  bool y116 = false;
  bool y117 = false;
  int y118 = 0;
  bool y119 = true;
  int y120 = 0;
  goto stop_process;
  stop_process: skip;
  child_115!0;
}
proctype fun14(Mutexdef mu121; chan child_122) {
  bool y123 = false;
  bool y124 = false;
  int y125 = 0;
  bool y126 = true;
  int y127 = 0;
  goto stop_process;
  stop_process: skip;
  child_122!0;
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