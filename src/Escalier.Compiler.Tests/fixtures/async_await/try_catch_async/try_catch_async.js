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
  var temp3;
  try {
    var y = await foo(x);
    temp3 = y + await 10;
  } catch (__error__) {
    var temp4;
    if (__error__ == "RangeError") {
      temp4 = 0;
    } else {
      throw __error__;
    }
  }
  return temp3;
};
