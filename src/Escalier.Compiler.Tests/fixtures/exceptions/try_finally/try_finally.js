var foo = (x) => {
  var temp0;
  if (x < 0) {
    temp0 = Escalier.throw("RangeError");
  } else {
    temp0 = x;
  }
  return temp0;
};
var cleanup = () => {
  return {};
};
var bar = (x) => {
  var temp1;
  try {
    temp1 = foo(x);
  } finally {
    cleanup();
  }
  return temp1;
};
