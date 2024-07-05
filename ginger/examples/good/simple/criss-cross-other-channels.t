c1 = chan [0];
c2 = chan [0];
c3 = chan [1];
go {
  go {
    for i1 : 0 .. x {
      c3!;
      c1!;
      c3?
    };
    for j2 : 0 .. x {
      c2?
    };
    for i3 : 0 .. x {
      c1!
    };
  };
  for j1 : 0 .. x {
    c1?
  };
  for i2 : 0 .. x {
    c2!
  };
  for j3 : 0 .. x {
    c1?
  };
}
