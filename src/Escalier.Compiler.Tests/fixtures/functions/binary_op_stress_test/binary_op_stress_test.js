var foo = function (temp0, temp1, temp2) {
  var a = temp0;
  var b = temp1;
  var c = temp2;
  return a * b + c;
};
var double = function (temp3) {
  var x = temp3;
  return 2 * x;
};
var inc = function (temp4) {
  var x = temp4;
  return x + 1;
};
var bar = foo(double(1), inc(2), 3);
var baz = 5;
var qux = double(baz);
