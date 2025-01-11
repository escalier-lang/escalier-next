var foo = (x) => {
  var temp0;
  if (x < 0) {
    temp0 = Escalier.throw("RangeError");
  } else {
    temp0 = x;
  }
  return temp0;
};
var bar = (x) => {
  return foo(x);
};
