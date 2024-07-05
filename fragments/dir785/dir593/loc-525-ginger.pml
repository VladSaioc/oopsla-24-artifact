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
  chan child_10 = [1] of {int};
  run fun2(child_10);
  child_10?0;
  stop_process: skip;
}
proctype fun2(chan child_11) {
  bool y12 = false;
  bool y13 = false;
  int y14 = 0;
  bool y15 = true;
  int y16 = 0;
  chan child_17 = [1] of {int};
  Wgdef w18 ;
  chan child_19 = [1] of {int};
  int y20 = x1;
  int y21 = 2;
  chan c22 = [y21] of {int};
  run fun3(c22, child_19);
  run receiver(child_19);
  run wg_monitor(w18);
  for(y14 : 0 .. (y20) - (1)) {
    for30: skip;
    run fun5(c22, w18, child_17);
    child_17?0;
    for30_end: skip;
  };
  for30_exit: skip;
  w18.wait?0;
  stop_process: skip;
  child_11!0;
}
proctype fun3(chan c23; chan child_24) {
  bool y25 = false;
  bool y26 = false;
  int y27 = 0;
  bool y28 = true;
  int y29 = 0;
  chan child_30 = [1] of {int};
  run fun4(c23, child_30);
  child_30?0;
  stop_process: skip;
  child_24!0;
}
proctype fun4(chan c31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  stop_process: skip;
  child_32!0;
}
proctype fun5(chan c38; Wgdef w39; chan child_40) {
  bool y41 = false;
  bool y42 = false;
  int y43 = 0;
  bool y44 = true;
  int y45 = 0;
  chan child_46 = [1] of {int};
  chan child_47 = [1] of {int};
  w39.update!2;
  w39.update_ack?y42;
  assert y42;
  run fun6(c38, w39, child_47);
  run receiver(child_47);
  run fun9(c38, w39, child_46);
  run receiver(child_46);
  stop_process: skip;
  child_40!0;
}
proctype fun6(chan c48; Wgdef w49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  chan child_56 = [1] of {int};
  chan child_57 = [1] of {int};
  chan child_58 = [1] of {int};
  chan child_59 = [1] of {int};
  run fun7(c48, child_59);
  child_59?0;
  run fun7(c48, child_58);
  child_58?0;
  run fun7(c48, child_57);
  child_57?0;
  run fun7(c48, child_56);
  child_56?0;
  defer1: skip;
  skip;
  w49.update!-(1);
  w49.update_ack?y52;
  assert y52;
  stop_process: skip;
  child_50!0;
}
proctype fun7(chan c60; chan child_61) {
  bool y62 = false;
  bool y63 = false;
  int y64 = 0;
  bool y65 = true;
  int y66 = 0;
  c60!0;
  stop_process: skip;
  child_61!0;
}
proctype fun8(chan c67; chan child_68) {
  bool y69 = false;
  bool y70 = false;
  int y71 = 0;
  bool y72 = true;
  int y73 = 0;
  c67?0;
  stop_process: skip;
  child_68!0;
}
proctype fun9(chan c74; Wgdef w75; chan child_76) {
  bool y77 = false;
  bool y78 = false;
  int y79 = 0;
  bool y80 = true;
  int y81 = 0;
  chan child_82 = [1] of {int};
  chan child_83 = [1] of {int};
  chan child_84 = [1] of {int};
  chan child_85 = [1] of {int};
  run fun8(c74, child_85);
  child_85?0;
  run fun8(c74, child_84);
  child_84?0;
  run fun8(c74, child_83);
  child_83?0;
  run fun8(c74, child_82);
  child_82?0;
  defer1: skip;
  skip;
  w75.update!-(1);
  w75.update_ack?y78;
  assert y78;
  stop_process: skip;
  child_76!0;
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
