// git_link=loc-298-ginger.pml
#define  default true
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
  chan child_3 = [1] of {int};
  run fun1(child_3);
  child_3?0;
  stop_process: skip;
}
proctype fun1(chan child_4) {
  bool y5 = false;
  bool y6 = false;
  int y7 = 0;
  bool y8 = true;
  int y9 = 0;
  chan child_10 = [1] of {int};
  Wgdef w11 ;
  chan c12 = [100] of {int};
  run wg_monitor(w11);
  run fun2(c12, w11, child_10);
  child_10?0;
  c12?0;
  stop_process: skip;
  child_4!0;
}
proctype fun2(chan c13; Wgdef w14; chan child_15) {
  bool y16 = false;
  bool y17 = false;
  int y18 = 0;
  bool y19 = true;
  int y20 = 0;
  int y21 = -(2);
  int y22 = -(2);
  int y23 = -(2);
  int y24 = -(2);
  if
  :: true ->
    c13!0;
    goto stop_process;
  :: true ->


  fi;
  c13!0;
  if
  :: true ->
    c13!0;
    goto stop_process;
  :: true ->


  fi;
  if
  :: ((y24) - (1)) != (-(3)) ->
    for(y18 : 0 .. (y24) - (1)) {
      for10: skip;
      if
      :: true ->
        c13!0;
        goto stop_process;
      :: true ->


      fi;
      if
      :: true ->
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: ((y23) - (1)) != (-(3)) ->
          for(y18 : 0 .. (y23) - (1)) {
            for13: skip;
            if
            :: true ->
              if
              :: ((y22) - (1)) != (-(3)) ->
                for(y18 : 0 .. (y22) - (1)) {
                  for14: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for15: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for15_end;
                      :: true ->


                      fi;
                      for15_end: skip;
                    };
                    for15_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for16: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for16_end;
                      :: true ->


                      fi;
                      for16_end: skip;
                    :: true ->
                      break;

                    od;
                    for16_exit: skip;
                  fi;
                  for14_end: skip;
                };
                for14_exit: skip;
              :: else  ->
                do
                :: true ->
                  for17: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for18: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for18_end;
                      :: true ->


                      fi;
                      for18_end: skip;
                    };
                    for18_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for19: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for19_end;
                      :: true ->


                      fi;
                      for19_end: skip;
                    :: true ->
                      break;

                    od;
                    for19_exit: skip;
                  fi;
                  for17_end: skip;
                :: true ->
                  break;

                od;
                for17_exit: skip;
              fi;
            :: true ->


            fi;
            for13_end: skip;
          };
          for13_exit: skip;
        :: else  ->
          do
          :: true ->
            for110: skip;
            if
            :: true ->
              if
              :: ((y22) - (1)) != (-(3)) ->
                for(y18 : 0 .. (y22) - (1)) {
                  for111: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for112: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for112_end;
                      :: true ->


                      fi;
                      for112_end: skip;
                    };
                    for112_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for113: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for113_end;
                      :: true ->


                      fi;
                      for113_end: skip;
                    :: true ->
                      break;

                    od;
                    for113_exit: skip;
                  fi;
                  for111_end: skip;
                };
                for111_exit: skip;
              :: else  ->
                do
                :: true ->
                  for114: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for115: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for115_end;
                      :: true ->


                      fi;
                      for115_end: skip;
                    };
                    for115_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for116: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for116_end;
                      :: true ->


                      fi;
                      for116_end: skip;
                    :: true ->
                      break;

                    od;
                    for116_exit: skip;
                  fi;
                  for114_end: skip;
                :: true ->
                  break;

                od;
                for114_exit: skip;
              fi;
            :: true ->


            fi;
            for110_end: skip;
          :: true ->
            break;

          od;
          for110_exit: skip;
        fi;
      :: true ->


      fi;
      c13!0;
      for10_end: skip;
    };
    for10_exit: skip;
  :: else  ->
    do
    :: true ->
      for117: skip;
      if
      :: true ->
        c13!0;
        goto stop_process;
      :: true ->


      fi;
      if
      :: true ->
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: true ->
          c13!0;
          goto stop_process;
        :: true ->


        fi;
        if
        :: ((y23) - (1)) != (-(3)) ->
          for(y18 : 0 .. (y23) - (1)) {
            for120: skip;
            if
            :: true ->
              if
              :: ((y22) - (1)) != (-(3)) ->
                for(y18 : 0 .. (y22) - (1)) {
                  for121: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for122: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for122_end;
                      :: true ->


                      fi;
                      for122_end: skip;
                    };
                    for122_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for123: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for123_end;
                      :: true ->


                      fi;
                      for123_end: skip;
                    :: true ->
                      break;

                    od;
                    for123_exit: skip;
                  fi;
                  for121_end: skip;
                };
                for121_exit: skip;
              :: else  ->
                do
                :: true ->
                  for124: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for125: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for125_end;
                      :: true ->


                      fi;
                      for125_end: skip;
                    };
                    for125_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for126: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for126_end;
                      :: true ->


                      fi;
                      for126_end: skip;
                    :: true ->
                      break;

                    od;
                    for126_exit: skip;
                  fi;
                  for124_end: skip;
                :: true ->
                  break;

                od;
                for124_exit: skip;
              fi;
            :: true ->


            fi;
            for120_end: skip;
          };
          for120_exit: skip;
        :: else  ->
          do
          :: true ->
            for127: skip;
            if
            :: true ->
              if
              :: ((y22) - (1)) != (-(3)) ->
                for(y18 : 0 .. (y22) - (1)) {
                  for128: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for129: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for129_end;
                      :: true ->


                      fi;
                      for129_end: skip;
                    };
                    for129_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for130: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for130_end;
                      :: true ->


                      fi;
                      for130_end: skip;
                    :: true ->
                      break;

                    od;
                    for130_exit: skip;
                  fi;
                  for128_end: skip;
                };
                for128_exit: skip;
              :: else  ->
                do
                :: true ->
                  for131: skip;
                  if
                  :: ((y21) - (1)) != (-(3)) ->
                    for(y18 : 0 .. (y21) - (1)) {
                      for132: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for132_end;
                      :: true ->


                      fi;
                      for132_end: skip;
                    };
                    for132_exit: skip;
                  :: else  ->
                    do
                    :: true ->
                      for133: skip;
                      if
                      :: true ->
                        c13!0;
                        goto for133_end;
                      :: true ->


                      fi;
                      for133_end: skip;
                    :: true ->
                      break;

                    od;
                    for133_exit: skip;
                  fi;
                  for131_end: skip;
                :: true ->
                  break;

                od;
                for131_exit: skip;
              fi;
            :: true ->


            fi;
            for127_end: skip;
          :: true ->
            break;

          od;
          for127_exit: skip;
        fi;
      :: true ->


      fi;
      c13!0;
      for117_end: skip;
    :: true ->
      break;

    od;
    for117_exit: skip;
  fi;
  instance: skip;
  c13!0;
  stop_process: skip;
  child_15!0;
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