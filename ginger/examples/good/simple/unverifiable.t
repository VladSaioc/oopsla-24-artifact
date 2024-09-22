if numLevels == 0 { return } else { skip };
w = WaitGroup;
c = chan [numLevels];
for i : 0 .. complianceLevels { w.Add(1) };
go {
  for j : 0 .. complianceLevels {
    c!; w.Add(0-1)
  }
};
w.Wait();
for k : 0 ..  numLevels {
  c?
}
