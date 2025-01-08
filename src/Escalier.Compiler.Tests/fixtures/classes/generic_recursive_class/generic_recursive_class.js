var Node = class  {
"value"
"left"
"right"
"map"(mapper) {
  var node = {value: mapper(self.value), left: self.left.map(mapper), right: self.right.map(mapper)};
}
};
