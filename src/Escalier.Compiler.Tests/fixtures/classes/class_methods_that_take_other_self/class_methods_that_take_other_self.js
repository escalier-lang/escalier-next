var Point = class {
  x
  y
  constructor(temp0, temp1) {
    var x = temp0;
    var y = temp1;
    self.x = x;
    self.y = y;
  }
  makePoint(temp2, temp3) {
    var x = temp2;
    var y = temp3;
    return new Self(x, y);
  }
  add(temp4) {
    var other = temp4;
    return Self.makePoint((self.x + other.x), (self.y + other.y));
  }
};
var p1 = new Point(1, 0);
var p2 = new Point(0, 1);
var p3 = p1.add(p2);
