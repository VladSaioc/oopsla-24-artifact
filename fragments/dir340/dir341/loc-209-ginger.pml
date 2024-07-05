// git_link=loc-209-ginger.pml
#define  default true
typedef Mutexdef {
  chan Lock = [0] of {bool};
  chan Unlock = [0] of {bool};
  chan RLock = [0] of {bool};
  chan RUnlock = [0] of {bool};
  int Counter = 0;
}
init {
  chan child_13 = [1] of {int};
  run fun1(child_13);
  child_13?0;
  stop_process: skip;
}
proctype fun1(chan child_14) {
  bool y15 = false;
  bool y16 = false;
  int y17 = 0;
  bool y18 = true;
  int y19 = 0;
  chan child_20 = [1] of {int};
  chan child_21 = [1] of {int};
  Mutexdef mu22 ;
  Mutexdef mu23 ;
  run mutex_monitor(mu23);
  run mutex_monitor(mu22);
  run fun2(mu23, mu22, child_21);
  child_21?0;
  run fun10(mu23, mu22, child_20);
  child_20?0;
  stop_process: skip;
  child_14!0;
}
proctype fun2(Mutexdef mu24; Mutexdef mu25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  chan child_32 = [1] of {int};
  run fun3(mu24, mu25, child_32);
  child_32?0;
  stop_process: skip;
  child_26!0;
}
proctype fun3(Mutexdef mu33; Mutexdef mu34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  chan child_41 = [1] of {int};
  chan child_42 = [1] of {int};
  int y43 = -(2);
  int y44 = -(2);
  mu33.RLock?y37;
  assert y37;
  mu33.RUnlock?y37;
  assert y37;
  if
  :: ((y43) - (1)) != (-(3)) ->
    for(y38 : 0 .. (y43) - (1)) {
      for20: skip;
      mu33.Lock?y37;
      assert y37;
      run fun4(mu33, mu34, child_41);
      child_41?0;
      mu33.Unlock?y37;
      assert y37;
      for20_end: skip;
    };
    for20_exit: skip;
  :: else  ->
    do
    :: true ->
      for27: skip;
      mu33.Lock?y37;
      assert y37;
      run fun4(mu33, mu34, child_42);
      child_42?0;
      mu33.Unlock?y37;
      assert y37;
      for27_end: skip;
    :: true ->
      break;

    od;
    for27_exit: skip;
  fi;
  stop_process: skip;
  child_35!0;
}
proctype fun4(Mutexdef mu46; Mutexdef mu47; chan child_48) {
  bool y49 = false;
  bool y50 = false;
  int y51 = 0;
  bool y52 = true;
  int y53 = 0;
  chan child_54 = [1] of {int};
  chan child_55 = [1] of {int};
  run fun5(mu46, mu47, child_55);
  child_55?0;
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
  run fun9(mu46, mu47, child_54);
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
proctype fun5(Mutexdef mu59; Mutexdef mu60; chan child_61) {
  bool y62 = false;
  bool y63 = false;
  int y64 = 0;
  bool y65 = true;
  int y66 = 0;
  chan child_67 = [1] of {int};
  run fun6(mu59, mu60, child_67);
  child_67?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_61!0;
}
proctype fun6(Mutexdef mu69; Mutexdef mu70; chan child_71) {
  bool y72 = false;
  bool y73 = false;
  int y74 = 0;
  bool y75 = true;
  int y76 = 0;
  int y77 = 0;
  chan child_78 = [1] of {int};
  run fun7(mu69, mu70, child_78);
  child_78?0;
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
  goto stop_process;
  stop_process: skip;
  child_71!0;
}
proctype fun7(Mutexdef mu82; Mutexdef mu83; chan child_84) {
  bool y85 = false;
  bool y86 = false;
  int y87 = 0;
  bool y88 = true;
  int y89 = 0;
  chan child_90 = [1] of {int};
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
  if
  :: true ->
    run fun8(mu82, mu83, child_90);
    child_90?0;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_84!0;
}
proctype fun8(Mutexdef mu95; Mutexdef mu96; chan child_97) {
  bool y98 = false;
  bool y99 = false;
  int y100 = 0;
  bool y101 = true;
  int y102 = 0;
  int y103 = -(2);
  int y104 = -(2);
  stop_process: skip;
  child_97!0;
}
proctype fun9(Mutexdef mu105; Mutexdef mu106; chan child_107) {
  bool y108 = false;
  bool y109 = false;
  int y110 = 0;
  bool y111 = true;
  int y112 = 0;
  chan child_113 = [1] of {int};
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  if
  :: true ->
    run fun5(mu105, mu106, child_113);
    child_113?0;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_107!0;
}
proctype fun10(Mutexdef mu116; Mutexdef mu117; chan child_118) {
  bool y119 = false;
  bool y120 = false;
  int y121 = 0;
  bool y122 = true;
  int y123 = 0;
  chan child_124 = [1] of {int};
  run fun11(mu116, mu117, child_124);
  child_124?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_118!0;
}
proctype fun11(Mutexdef mu126; Mutexdef mu127; chan child_128) {
  bool y129 = false;
  bool y130 = false;
  int y131 = 0;
  bool y132 = true;
  int y133 = 0;
  chan child_134 = [1] of {int};
  run fun12(mu126, mu127, child_134);
  child_134?0;
  goto stop_process;
  stop_process: skip;
  child_128!0;
}
proctype fun12(Mutexdef mu135; Mutexdef mu136; chan child_137) {
  bool y138 = false;
  bool y139 = false;
  int y140 = 0;
  bool y141 = true;
  int y142 = 0;
  chan child_143 = [1] of {int};
  mu135.Lock?y139;
  assert y139;
  run fun5(mu135, mu136, child_143);
  child_143?0;
  defer1: skip;
  skip;
  mu135.Unlock?y139;
  assert y139;
  stop_process: skip;
  child_137!0;
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