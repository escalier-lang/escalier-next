var foo = function (a, b, c) {
  return a * b + c;
};
var double = function (x) {
  return 2 * x;
};
var inc = function (x) {
  return x + 1;
};
var bar = foo(double(1), inc(2), 3);
var baz = 5;
var qux = double(baz);
