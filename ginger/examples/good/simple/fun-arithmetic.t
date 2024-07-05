c = chan [k];
go {
  go {
    for j : 0 .. x {
      c?;
      c?
    }
  };
  for i1 : 0 .. x {
    c!
  };
  for i2 : 0 .. x {
    c!
  }
}
