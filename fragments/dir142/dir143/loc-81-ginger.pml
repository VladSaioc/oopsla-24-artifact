// git_link=loc-81-ginger.pml
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
  chan child_5 = [1] of {int};
  run fun2(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  chan child_15 = [1] of {int};
  int y16 = 0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [0] of {int};
  chan c19 = [0] of {int};
  run fun3(c18, c19, child_15);
  run receiver(child_15);
  run wg_monitor(w14);
  w14.update!y6;
  w14.update_ack?y9;
  assert y9;
  for(y10 : 0 .. (y6) - (1)) {
    for20: skip;
    run fun4(c18, c19, w14, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  w14.wait?0;
  c19!0;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun3(chan c21; chan c22; chan child_23) {
  bool y24 = false;
  bool y25 = false;
  int y26 = 0;
  bool y27 = true;
  int y28 = 0;
  stop_process: skip;
  child_23!0;
}
proctype fun4(chan c29; chan c30; Wgdef w31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  if
  :: true ->
    c29!0;
  :: true ->


  fi;
  defer1: skip;
  skip;
  w31.update!-(1);
  w31.update_ack?y34;
  assert y34;
  stop_process: skip;
  child_32!0;
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