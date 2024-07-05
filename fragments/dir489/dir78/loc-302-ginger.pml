// git_link=loc-302-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
}
init {
  chan child_13 = [1] of {int};
  run fun3(x1, x2, child_13);
  child_13?0;
  stop_process: skip;
}
proctype fun3(int y14; int y15; chan child_16) {
  bool y17 = false;
  bool y18 = false;
  int y19 = 0;
  bool y20 = true;
  int y21 = 0;
  chan child_22 = [1] of {int};
  chan child_23 = [1] of {int};
  chan child_24 = [1] of {int};
  chan child_25 = [1] of {int};
  chan child_26 = [1] of {int};
  chan child_27 = [1] of {int};
  chan child_28 = [1] of {int};
  chan child_29 = [1] of {int};
  chan child_30 = [1] of {int};
  chan child_31 = [1] of {int};
  chan child_32 = [1] of {int};
  Mutexdef mu33 ;
  int y34 = -(2);
  int y35 = -(2);
  int y36 = -(2);
  run mutex_monitor(mu33);
  run fun4(mu33, child_32);
  child_32?0;
  if
  :: ((y14) - (1)) != (-(3)) ->
    for(y19 : 0 .. (y14) - (1)) {
      for10: skip;
      if
      :: true ->
        if
        :: ((y36) - (1)) != (-(3)) ->
          for(y19 : 0 .. (y36) - (1)) {
            for11: skip;
            run fun5(mu33, child_25);
            child_25?0;
            for11_end: skip;
          };
          for11_exit: skip;
        :: else  ->
          do
          :: true ->
            for12: skip;
            run fun5(mu33, child_26);
            child_26?0;
            for12_end: skip;
          :: true ->
            break;

          od;
          for12_exit: skip;
        fi;
      :: true ->


      fi;
      if
      :: true ->
        if
        :: ((y35) - (1)) != (-(3)) ->
          for(y19 : 0 .. (y35) - (1)) {
            for13: skip;
            run fun6(mu33, child_23);
            child_23?0;
            for13_end: skip;
          };
          for13_exit: skip;
        :: else  ->
          do
          :: true ->
            for14: skip;
            run fun6(mu33, child_24);
            child_24?0;
            for14_end: skip;
          :: true ->
            break;

          od;
          for14_exit: skip;
        fi;
      :: true ->


      fi;
      if
      :: true ->
        run fun7(mu33, child_22);
        child_22?0;
      :: true ->


      fi;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for115: skip;
      if
      :: true ->
        if
        :: ((y36) - (1)) != (-(3)) ->
          for(y19 : 0 .. (y36) - (1)) {
            for116: skip;
            run fun5(mu33, child_30);
            child_30?0;
            for116_end: skip;
          };
          for116_exit: skip;
        :: else  ->
          do
          :: true ->
            for117: skip;
            run fun5(mu33, child_31);
            child_31?0;
            for117_end: skip;
          :: true ->
            break;

          od;
          for117_exit: skip;
        fi;
      :: true ->


      fi;
      if
      :: true ->
        if
        :: ((y35) - (1)) != (-(3)) ->
          for(y19 : 0 .. (y35) - (1)) {
            for118: skip;
            run fun6(mu33, child_28);
            child_28?0;
            for118_end: skip;
          };
          for118_exit: skip;
        :: else  ->
          do
          :: true ->
            for119: skip;
            run fun6(mu33, child_29);
            child_29?0;
            for119_end: skip;
          :: true ->
            break;

          od;
          for119_exit: skip;
        fi;
      :: true ->


      fi;
      if
      :: true ->
        run fun7(mu33, child_27);
        child_27?0;
      :: true ->


      fi;
      for115_end: skip;
    :: true ->
      break;

    od;
    for115_exit: skip;
  fi;
  stop_process: skip;
  child_16!0;
}
proctype fun4(Mutexdef mu43; chan child_44) {
  bool y45 = false;
  bool y46 = false;
  int y47 = 0;
  bool y48 = true;
  int y49 = 0;
  goto stop_process;
  stop_process: skip;
  child_44!0;
}
proctype fun5(Mutexdef mu50; chan child_51) {
  bool y52 = false;
  bool y53 = false;
  int y54 = 0;
  bool y55 = true;
  int y56 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  stop_process: skip;
  child_51!0;
}
proctype fun6(Mutexdef mu58; chan child_59) {
  bool y60 = false;
  bool y61 = false;
  int y62 = 0;
  bool y63 = true;
  int y64 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  stop_process: skip;
  child_59!0;
}
proctype fun7(Mutexdef mu66; chan child_67) {
  bool y68 = false;
  bool y69 = false;
  int y70 = 0;
  bool y71 = true;
  int y72 = 0;
  chan child_73 = [1] of {int};
  chan child_74 = [1] of {int};
  if
  :: true ->
    goto stop_process;

  fi;
  run fun8(mu66, child_74);
  child_74?0;
  run fun12(mu66, child_73);
  child_73?0;
  stop_process: skip;
  child_67!0;
}
proctype fun8(Mutexdef mu76; chan child_77) {
  bool y78 = false;
  bool y79 = false;
  int y80 = 0;
  bool y81 = true;
  int y82 = 0;
  chan child_83 = [1] of {int};
  chan child_84 = [1] of {int};
  chan child_85 = [1] of {int};
  chan child_86 = [1] of {int};
  if
  :: true ->

  :: true ->


  fi;
  mu76.Lock?y79;
  assert y79;
  if
  :: true ->
    run fun9(mu76, child_84);
    child_84?0;
  :: true ->
    run fun10(mu76, child_85);
    child_85?0;
  :: true ->
    run fun10(mu76, child_86);
    child_86?0;

  fi;
  run fun11(mu76, child_83);
  child_83?0;
  defer1: skip;
  skip;
  mu76.Unlock?y79;
  assert y79;
  stop_process: skip;
  child_77!0;
}
proctype fun9(Mutexdef mu89; chan child_90) {
  bool y91 = false;
  bool y92 = false;
  int y93 = 0;
  bool y94 = true;
  int y95 = 0;
  int y96 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  stop_process: skip;
  child_90!0;
}
proctype fun10(Mutexdef mu98; chan child_99) {
  bool y100 = false;
  bool y101 = false;
  int y102 = 0;
  bool y103 = true;
  int y104 = 0;
  int y105 = 0;
  int y106 = -(2);
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  stop_process: skip;
  child_99!0;
}
proctype fun11(Mutexdef mu108; chan child_109) {
  bool y110 = false;
  bool y111 = false;
  int y112 = 0;
  bool y113 = true;
  int y114 = 0;
  stop_process: skip;
  child_109!0;
}
proctype fun12(Mutexdef mu115; chan child_116) {
  bool y117 = false;
  bool y118 = false;
  int y119 = 0;
  bool y120 = true;
  int y121 = 0;
  int y122 = -(2);
  stop_process: skip;
  child_116!0;
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