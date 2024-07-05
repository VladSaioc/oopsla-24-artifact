// git_link=loc-398-ginger.pml
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
  if
  :: (y7) <= (0) ->
    assert (20) == (0);
  :: else  ->
  fi;
  chan c16 = [y7] of {int};
  run fun4(c16, child_14);
  child_14?0;
  goto stop_process;
  stop_process: skip;
  child_8!0;
}
proctype fun4(chan c17; chan child_18) {
  bool y19 = false;
  bool y20 = false;
  int y21 = 0;
  bool y22 = true;
  int y23 = 0;
  int y24 = -(2);
  if
  :: ((0) != (-(2))) && (((y24) - (1)) != (-(3))) ->
    for(y21 : 0 .. (y24) - (1)) {
      for21: skip;
      if
      :: true ->
        assert (20) == (0);
      :: true ->


      fi;
      if
      :: true ->
        if
        :: true ->
          assert (20) == (0);
        :: true ->


        fi;
      :: true ->


      fi;
      c17!0;
      for21_end: skip;
    };
    for21_exit: skip;
  :: else  ->
    do
    :: true ->
      for20: skip;
      if
      :: true ->
        assert (20) == (0);
      :: true ->


      fi;
      if
      :: true ->
        if
        :: true ->
          assert (20) == (0);
        :: true ->


        fi;
      :: true ->


      fi;
      c17!0;
      for20_end: skip;
    :: true ->
      break;

    od;
    for20_exit: skip;
  fi;
  stop_process: skip;
  child_18!0;
}