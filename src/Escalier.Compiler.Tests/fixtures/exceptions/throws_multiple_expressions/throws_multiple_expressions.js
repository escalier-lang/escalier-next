var foo = (x) => {
  var temp0;
  if (x < 0) {
    temp0 = Escalier.throw("RangeError");
  } else {
    temp0 = Escalier.throw("BoundsError");
  }
  return temp0;
};
