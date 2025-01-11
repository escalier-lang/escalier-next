var Point = class  {
x
y
constructor(x, y) {
  self.x = x;
  self.y = y;
}
makePoint(x, y) {
  return Self(x, y);
}
add(other) {
  return Self.makePoint((self.x + other.x), (self.y + other.y));
}
};
var p1 = Point(1, 0);
var p2 = Point(0, 1);
var p3 = p1.add(p2);
