var foo = (temp0) => {
  var x = temp0;
  var temp1;
  if (x < 0) {
    temp1 = Escalier.throw("RangeError");
  } else {
    temp1 = x;
  }
  return temp1;
};
var bar = (temp2) => {
  var x = temp2;
  return foo(x);
};
