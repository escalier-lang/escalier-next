var foo = (x) => {
  var temp0;
  if (x < 0) {
    temp0 = Escalier.throw("RangeError");
  } else {
    temp0 = Escalier.throw("BoundsError");
  }
  return temp0;
};
var bar = (x) => {
  var temp1;
  try {
    temp1 = foo(x);
  } catch (__error__) {
    var temp2;
    if (__error__ == "RangeError") {
      temp2 = 0;
    } else {
      var temp3;
      if (__error__ == "BoundsError") {
        temp3 = 0;
      } else {
        throw __error__;
      }
      temp2 = temp3;
    }
  }
  return temp1;
};
