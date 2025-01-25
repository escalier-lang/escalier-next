var temp0;
if (typeof point == "object" && "x" in point && "y" in point) {
  var {x, y} = point;
  temp0 = x + y;
} else {
  temp0 = 0;
}
var sum = temp0;
