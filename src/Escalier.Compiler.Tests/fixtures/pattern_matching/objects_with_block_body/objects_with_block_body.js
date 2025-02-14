var temp0;
if (typeof shape == "object" && "type" in shape && shape["type"] == "circle") {
  var {...rest} = shape;
  temp0 = rest.center;
} else {
  var temp1;
  if (typeof shape == "object" && "type" in shape && shape["type"] == "line" && "start" in shape && "end" in shape) {
    var {start, end} = shape;
    var x = (start.x + end.x) / 2;
    var y = (start.y + end.y) / 2;
    temp1 = {x, y};
  }
  temp0 = temp1;
}
var centroid = temp0;
