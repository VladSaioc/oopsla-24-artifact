c = chan [k - 1];
for i : 0 .. x {
  c!;
};
go {
  for j : 0 .. y {
    c?;
  }
}
