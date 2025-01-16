var isEven = (n) => {
  var temp0;
  if (n == 0) {
    temp0 = true;
  } else {
    temp0 = isOdd((n - 1));
  }
  return temp0;
};
var isOdd = (n) => {
  var temp1;
  if (n == 0) {
    temp1 = false;
  } else {
    temp1 = isEven((n - 1));
  }
  return temp1;
};
