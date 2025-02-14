var temp0;
if (value.length == 2) {
  var [a, _] = value;
  temp0 = a;
} else {
  var temp1;
  if (typeof value == "object" && "b" in value) {
    var {b} = value;
    temp1 = b;
  }
  temp0 = temp1;
}
var result = temp0;
