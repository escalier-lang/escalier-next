var temp0;
if (value.length == 2) {
  var [a, b] = value;
  temp0 = a;
} else {
  var temp1;
  if (typeof value == "object" && "a" in value && "b" in value) {
    var {a, b} = value;
    temp1 = a;
  }
  temp0 = temp1;
}
var result = temp0;
