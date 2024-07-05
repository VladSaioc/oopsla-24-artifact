if x <= 0 {
  return
} else {
  skip
};
c = chan [x];
go {
  for i : 0 .. x {
    c!
  }
};
c?
