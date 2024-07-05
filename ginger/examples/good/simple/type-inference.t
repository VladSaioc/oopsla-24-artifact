c = chan [0];
go {
  if a == b {
    go {
      for i : 0 .. x {
        c?
      }
    }
  } else { skip };
  go {
    if x == y {
      for j : 0 .. y {
        c!
      }
    } else { skip }
  }
}
