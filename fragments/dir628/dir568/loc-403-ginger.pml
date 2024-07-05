// git_link=loc-403-ginger.pml
#define  default true
#define  x1 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_8 = [1] of {int};
  run fun2(child_8);
  child_8?0;
  stop_process: skip;
}
proctype fun2(chan child_9) {
  bool y10 = false;
  bool y11 = false;
  int y12 = 0;
  bool y13 = true;
  int y14 = 0;
  chan child_15 = [1] of {int};
  chan child_16 = [1] of {int};
  chan child_17 = [1] of {int};
  chan child_18 = [1] of {int};
  chan child_19 = [1] of {int};
  Wgdef w20 ;
  int y21 = -(2);
  int y22 = -(2);
  int y23 = -(2);
  int y24 = x1;
  run wg_monitor(w20);
  w20.update!y24;
  w20.update_ack?y11;
  assert y11;
  run fun3(w20, child_19);
  run receiver(child_19);
  run fun4(w20, child_18);
  run receiver(child_18);
  run fun5(w20, child_17);
  run receiver(child_17);
  run fun6(w20, child_16);
  run receiver(child_16);
  run fun7(w20, child_15);
  run receiver(child_15);
  w20.wait?0;
  if
  :: true ->
    if
    :: true ->

    :: true ->


    fi;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_9!0;
}
proctype fun3(Wgdef w27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  defer1: skip;
  skip;
  w27.update!-(1);
  w27.update_ack?y30;
  assert y30;
  stop_process: skip;
  child_28!0;
}
proctype fun4(Wgdef w34; chan child_35) {
  bool y36 = false;
  bool y37 = false;
  int y38 = 0;
  bool y39 = true;
  int y40 = 0;
  defer1: skip;
  skip;
  w34.update!-(1);
  w34.update_ack?y37;
  assert y37;
  stop_process: skip;
  child_35!0;
}
proctype fun5(Wgdef w41; chan child_42) {
  bool y43 = false;
  bool y44 = false;
  int y45 = 0;
  bool y46 = true;
  int y47 = 0;
  defer1: skip;
  skip;
  w41.update!-(1);
  w41.update_ack?y44;
  assert y44;
  stop_process: skip;
  child_42!0;
}
proctype fun6(Wgdef w48; chan child_49) {
  bool y50 = false;
  bool y51 = false;
  int y52 = 0;
  bool y53 = true;
  int y54 = 0;
  defer1: skip;
  skip;
  w48.update!-(1);
  w48.update_ack?y51;
  assert y51;
  stop_process: skip;
  child_49!0;
}
proctype fun7(Wgdef w55; chan child_56) {
  bool y57 = false;
  bool y58 = false;
  int y59 = 0;
  bool y60 = true;
  int y61 = 0;
  defer1: skip;
  skip;
  w55.update!-(1);
  w55.update_ack?y58;
  assert y58;
  stop_process: skip;
  child_56!0;
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