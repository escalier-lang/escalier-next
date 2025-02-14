var Node = class {
  value
  left
  right
  map(temp0) {
    var mapper = temp0;
    var node = {value: mapper(self.value), left: self.left.map(mapper), right: self.right.map(mapper)};
  }
};
