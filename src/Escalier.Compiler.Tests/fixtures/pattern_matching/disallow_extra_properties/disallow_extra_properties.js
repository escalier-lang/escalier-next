var temp0;
if (typeof value == "object" && "a" in value && "b" in value && "c" in value) {
  var {a, b: _, c: _} = value;
  temp0 = a;
}
var result = temp0;
