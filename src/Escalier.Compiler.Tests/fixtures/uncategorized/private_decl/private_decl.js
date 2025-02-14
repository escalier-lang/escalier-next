var makePoint = function (temp0, temp1) {
  var x = temp0;
  var y = temp1;
  var point = {x, y};
  return point;
};
var p = makePoint(5, 10);
var {x, y} = p;
