// git_link=loc-525-ginger.pml
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
init {
  chan child_11 = [1] of {int};
  run fun2(child_11);
  child_11?0;
  stop_process: skip;
}
proctype fun2(chan child_12) {
  bool y13 = false;
  bool y14 = false;
  int y15 = 0;
  bool y16 = true;
  int y17 = 0;
  chan child_18 = [1] of {int};
  Wgdef w19 ;
  chan child_20 = [1] of {int};
  int y21 = x1;
  int y22 = 0;
  chan c23 = [y22] of {int};
  run fun3(c23, child_20);
  run receiver(child_20);
  run wg_monitor(w19);
  for(y15 : 0 .. (y21) - (1)) {
    for30: skip;
    run fun5(c23, w19, child_18);
    child_18?0;
    for30_end: skip;
  };
  for30_exit: skip;
  w19.wait?0;
  stop_process: skip;
  child_12!0;
}
proctype fun3(chan c24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  chan child_31 = [1] of {int};
  run fun4(c24, child_31);
  child_31?0;
  stop_process: skip;
  child_25!0;
}
proctype fun4(chan c32; chan child_33) {
  bool y34 = false;
  bool y35 = false;
  int y36 = 0;
  bool y37 = true;
  int y38 = 0;
  stop_process: skip;
  child_33!0;
}
proctype fun5(chan c39; Wgdef w40; chan child_41) {
  bool y42 = false;
  bool y43 = false;
  int y44 = 0;
  bool y45 = true;
  int y46 = 0;
  chan child_47 = [1] of {int};
  chan child_48 = [1] of {int};
  w40.update!2;
  w40.update_ack?y43;
  assert y43;
  run fun6(c39, w40, child_48);
  run receiver(child_48);
  run fun9(c39, w40, child_47);
  run receiver(child_47);
  stop_process: skip;
  child_41!0;
}
proctype fun6(chan c49; Wgdef w50; chan child_51) {
  bool y52 = false;
  bool y53 = false;
  int y54 = 0;
  bool y55 = true;
  int y56 = 0;
  chan child_57 = [1] of {int};
  chan child_58 = [1] of {int};
  chan child_59 = [1] of {int};
  chan child_60 = [1] of {int};
  run fun7(c49, child_60);
  child_60?0;
  run fun7(c49, child_59);
  child_59?0;
  run fun7(c49, child_58);
  child_58?0;
  run fun7(c49, child_57);
  child_57?0;
  defer1: skip;
  skip;
  w50.update!-(1);
  w50.update_ack?y53;
  assert y53;
  stop_process: skip;
  child_51!0;
}
proctype fun7(chan c61; chan child_62) {
  bool y63 = false;
  bool y64 = false;
  int y65 = 0;
  bool y66 = true;
  int y67 = 0;
  c61!0;
  stop_process: skip;
  child_62!0;
}
proctype fun8(chan c68; Wgdef w69; chan child_70) {
  bool y71 = false;
  bool y72 = false;
  int y73 = 0;
  bool y74 = true;
  int y75 = 0;
  chan child_76 = [1] of {int};
  chan child_77 = [1] of {int};
  chan child_78 = [1] of {int};
  chan child_79 = [1] of {int};
  run fun7(c68, child_79);
  child_79?0;
  run fun7(c68, child_78);
  child_78?0;
  run fun7(c68, child_77);
  child_77?0;
  run fun7(c68, child_76);
  child_76?0;
  defer1: skip;
  skip;
  w69.update!-(1);
  w69.update_ack?y72;
  assert y72;
  stop_process: skip;
  child_70!0;
}
proctype fun9(chan c80; Wgdef w81; chan child_82) {
  bool y83 = false;
  bool y84 = false;
  int y85 = 0;
  bool y86 = true;
  int y87 = 0;
  chan child_88 = [1] of {int};
  chan child_89 = [1] of {int};
  chan child_90 = [1] of {int};
  chan child_91 = [1] of {int};
  run fun10(c80, child_91);
  child_91?0;
  run fun10(c80, child_90);
  child_90?0;
  run fun10(c80, child_89);
  child_89?0;
  run fun10(c80, child_88);
  child_88?0;
  defer1: skip;
  skip;
  w81.update!-(1);
  w81.update_ack?y84;
  assert y84;
  stop_process: skip;
  child_82!0;
}
proctype fun10(chan c92; chan child_93) {
  bool y94 = false;
  bool y95 = false;
  int y96 = 0;
  bool y97 = true;
  int y98 = 0;
  c92?0;
  stop_process: skip;
  child_93!0;
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
proctype receiver(chan c) {
  c?0;
}
