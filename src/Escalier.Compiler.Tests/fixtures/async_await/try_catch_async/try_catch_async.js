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
  var temp1;
  try {
    var y = await foo(x);
    temp1 = y + await 10;
  } catch (__error__) {
    var temp2;
    if (__error__ == "RangeError") {
      temp2 = 0;
    } else {
      throw __error__;
    }
  }
  return temp1;
};
