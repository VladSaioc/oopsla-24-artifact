// git_link=loc-372-ginger.pml
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
  Wgdef w16 ;
  run wg_monitor(w16);
  chan c17 = [100] of {int};
  chan c18 = [1] of {int};
  chan c19 = [0] of {int};
  chan c20 = [1] of {int};
  run fun3(c17, c18, c19, c20, w16, child_15);
  run receiver(child_15);
  run fun5(w16, c17, c18, c19, c20, child_14);
  run receiver(child_14);
  c19?0;
  w16.wait?0;
  c18!0;
  c20?0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun3(chan c22; chan c23; chan c24; chan c25; Wgdef w26; chan child_27) {
  bool y28 = false;
  bool y29 = false;
  int y30 = 0;
  bool y31 = true;
  int y32 = 0;
  chan child_33 = [1] of {int};
  run fun4(c22, w26, child_33);
  child_33?0;
  c25!0;
  stop_process: skip;
  child_27!0;
}
proctype fun4(chan c34; Wgdef w35; chan child_36) {
  bool y37 = false;
  bool y38 = false;
  int y39 = 0;
  bool y40 = true;
  int y41 = 0;
  for(y39 : 0 .. (x1) - (1)) {
    for10: skip;
    if
    :: true ->
      goto stop_process;
    :: true ->


    fi;
    if
    :: true ->
      w35.update!1;
      w35.update_ack?y38;
      assert y38;
      c34!0;
    :: true ->


    fi;
    if
    :: true ->
      break;
    :: true ->


    fi;
    for10_end: skip;
  };
  for10_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_36!0;
}
proctype fun5(Wgdef w45; chan c46; chan c47; chan c48; chan c49; chan child_50) {
  bool y51 = false;
  bool y52 = false;
  int y53 = 0;
  bool y54 = true;
  int y55 = 0;
  for20_exit: skip;
  stop_process: skip;
  child_50!0;
}
proctype fun6(Wgdef w56; chan child_57) {
  bool y58 = false;
  bool y59 = false;
  int y60 = 0;
  bool y61 = true;
  int y62 = 0;
  defer1: skip;
  skip;
  w56.update!-(1);
  w56.update_ack?y59;
  assert y59;
  stop_process: skip;
  child_57!0;
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