c1 = chan [0];
c2 = chan [0];
go {
  go {
    c1!
  };
  go {
    c2!
  };
  c1?;
  if b1 {
    return
  } else {
    skip
  };
  c2?;
}
