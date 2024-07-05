c1 = chan [k];
c2 = chan [k * 2];
c3 = chan [k2];
go {
  for j1 : 0 .. y1 {
    c1?;
  };
  for j2 : 0 .. y2 {
    c2!;
  };
  for j3 : 0 .. y3 {
    c2!;
  }
};
go {
  for k1 : 0 .. z3 {
    c3?;
  };
  for k2 : 0 .. z3 {
    c2?;
  };
  for k3 : 0 .. z3 {
    c2?;
  };
  for k4 : 0 .. z3 {
    c2?;
  };
  for k5 : 0 .. z3 {
    c2?;
  }
};
for i1 : x1 .. x1 * 2 {
  c1!;
  c1!;
};
for i2 : 0 .. x2 {
  c3!;
};
for i3 : 0-10 .. x3 {
  c2?;
}
