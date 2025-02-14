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
var cleanup = () => {
  return {};
};
var bar = (temp2) => {
  var x = temp2;
  var temp3;
  try {
    temp3 = foo(x);
  } finally {
    cleanup();
  }
  return temp3;
};
