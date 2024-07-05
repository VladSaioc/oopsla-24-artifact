// git_link=loc-77-ginger.pml
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
  run fun2(child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun2(chan child_6) {
  bool y7 = false;
  bool y8 = false;
  int y9 = 0;
  bool y10 = true;
  int y11 = 0;
  chan child_12 = [1] of {int};
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x1;
  chan c16 = [0] of {int};
  chan c17 = [0] of {int};
  run wg_monitor(w14);
  run fun3(c16, c17, w14, child_13);
  run receiver(child_13);
  for(y9 : 0 .. (y15) - (1)) {
    for20: skip;
    w14.update!1;
    w14.update_ack?y8;
    assert y8;
    run fun4(c16, c17, w14, child_12);
    run receiver(child_12);
    for20_end: skip;
  };
  for20_exit: skip;
  w14.wait?0;
  c17!0;
  goto stop_process;
  stop_process: skip;
  child_6!0;
}
proctype fun3(chan c18; chan c19; Wgdef w20; chan child_21) {
  bool y22 = false;
  bool y23 = false;
  int y24 = 0;
  bool y25 = true;
  int y26 = 0;
  do
  :: true ->
    if
    :: c18?y24 ->

    :: c19?y24 ->
      goto stop_process;

    fi;
    for20_end: skip;

  od;
  for20_exit: skip;
  stop_process: skip;
  child_21!0;
}
proctype fun4(chan c29; chan c30; Wgdef w31; chan child_32) {
  bool y33 = false;
  bool y34 = false;
  int y35 = 0;
  bool y36 = true;
  int y37 = 0;
  int y38 = -(2);
  c29!0;
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