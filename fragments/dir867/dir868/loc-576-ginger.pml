// git_link=loc-576-ginger.pml
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
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_6 = [1] of {int};
  run fun4(x1, child_6);
  child_6?0;
  stop_process: skip;
}
proctype fun4(int y7; chan child_8) {
  bool y9 = false;
  bool y10 = false;
  int y11 = 0;
  bool y12 = true;
  int y13 = 0;
  chan child_14 = [1] of {int};
  Wgdef w15 ;
  int y16 = x2;
  if
  :: true ->
    goto stop_process;
  :: true ->


  fi;
  chan c18 = [y7] of {int};
  run wg_monitor(w15);
  for(y11 : 0 .. (x3) - (1)) {
    for10: skip;
    if
    :: true ->
      goto for10_end;
    :: true ->


    fi;
    for(y11 : 0 .. (y16) - (1)) {
      for11: skip;
      if
      :: true ->
        if
        :: true ->
          if
          :: true ->
            if
            :: true ->
              c18!0;
              w15.update!1;
              w15.update_ack?y10;
              assert y10;
              run fun5(c18, w15, child_14);
              run receiver(child_14);
            :: true ->


            fi;
          :: true ->


          fi;
        :: true ->


        fi;

      fi;
      for11_end: skip;
    };
    for11_exit: skip;
    for10_end: skip;
  };
  for10_exit: skip;
  w15.wait?0;
  run close(c18);
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun5(chan c24; Wgdef w25; chan child_26) {
  bool y27 = false;
  bool y28 = false;
  int y29 = 0;
  bool y30 = true;
  int y31 = 0;
  c24?0;
  w25.update!-(1);
  w25.update_ack?y28;
  assert y28;
  stop_process: skip;
  child_26!0;
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