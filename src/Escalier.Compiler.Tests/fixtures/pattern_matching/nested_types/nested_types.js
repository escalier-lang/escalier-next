var temp0;
if (typeof value == "object" && "a" in value && value["a"].length == 1) {
  var {a: [x]} = value;
  temp0 = x;
} else {
  var temp1;
  if (typeof value == "object" && "a" in value && typeof value["a"] == "object" && "x" in value["a"]) {
    var {a: {x}} = value;
    temp1 = x;
  } else {
    var temp2;
    if (typeof value == "object" && "b" in value && value["b"].length == 1) {
      var {b: [y]} = value;
      temp2 = y;
    } else {
      var temp3;
      if (typeof value == "object" && "b" in value && typeof value["b"] == "object" && "y" in value["b"]) {
        var {b: {y}} = value;
        temp3 = y;
      }
      temp2 = temp3;
    }
    temp1 = temp2;
  }
  temp0 = temp1;
}
var result = temp0;
