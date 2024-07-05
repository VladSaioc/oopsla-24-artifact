// git_link=loc-442-ginger.pml
#define  default true
#define  x1 ??
#define  x2 ??
typedef Wgdef {
  chan update = [0] of {int};
  chan update_ack = [0] of {bool};
  chan wait = [0] of {int};
  int Counter = 0;
}
init {
  chan child_5 = [1] of {int};
  run fun3(x1, child_5);
  child_5?0;
  stop_process: skip;
}
proctype fun3(int y6; chan child_7) {
  bool y8 = false;
  bool y9 = false;
  int y10 = 0;
  bool y11 = true;
  int y12 = 0;
  chan child_13 = [1] of {int};
  Wgdef w14 ;
  int y15 = x2;
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
  run wg_monitor(w14);
  for(y10 : 0 .. (y6) - (1)) {
    for20: skip;
    if
    :: true ->
      goto for20_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for20_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for20_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for20_end;
    :: true ->


    fi;
    if
    :: true ->
      goto for20_end;
    :: true ->


    fi;
    w14.update!1;
    w14.update_ack?y9;
    assert y9;
    run fun4(w14, child_13);
    run receiver(child_13);
    for20_end: skip;
  };
  for20_exit: skip;
  w14.wait?0;
  goto stop_process;
  stop_process: skip;
  child_7!0;
}
proctype fun4(Wgdef w24; chan child_25) {
  bool y26 = false;
  bool y27 = false;
  int y28 = 0;
  bool y29 = true;
  int y30 = 0;
  defer1: skip;
  skip;
  w24.update!-(1);
  w24.update_ack?y27;
  assert y27;
  stop_process: skip;
  child_25!0;
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