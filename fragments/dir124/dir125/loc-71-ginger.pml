// git_link=loc-71-ginger.pml
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
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_5 = [1] of {int};
  run fun3(x1, x2, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  Wgdef w16 ;
  chan c17 = [y6] of {int};
  if
  :: ((0) != (-(2))) && (((y6) - (1)) != (-(3))) ->
    for(y11 : 0 .. (y6) - (1)) {
      for11: skip;
      c17!0;
      for11_end: skip;
    };
    for11_exit: skip;
  :: else  ->
    do
    :: true ->
      for10: skip;
      c17!0;
      for10_end: skip;
    :: true ->
      break;

    od;
    for10_exit: skip;
  fi;
  chan c19 = [0] of {int};
  run wg_monitor(w16);
  run wg_monitor(w15);
  chan c20 = [0] of {int};
  chan c21 = [0] of {int};
  for(y11 : 0 .. (y7) - (1)) {
    for20: skip;
    w15.update!1;
    w15.update_ack?y10;
    assert y10;
    w16.update!1;
    w16.update_ack?y10;
    assert y10;
    run fun4(c19, c20, c21, c17, w16, w15, child_14);
    run receiver(child_14);
    for20_end: skip;
  };
  for20_exit: skip;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c22; chan c23; chan c24; chan c25; Wgdef w26; Wgdef w27; chan child_28) {
  bool y29 = false;
  bool y30 = false;
  int y31 = 0;
  bool y32 = true;
  int y33 = 0;
  w27.update!-(1);
  w27.update_ack?y30;
  assert y30;
  c24?0;
  defer1: skip;
  skip;
  w26.update!-(1);
  w26.update_ack?y30;
  assert y30;
  stop_process: skip;
  child_28!0;
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