var factorial = (temp0) => {
  var n = temp0;
  var temp1;
  if (n == 0) {
    temp1 = 1;
  } else {
    temp1 = n * factorial((n - 1));
  }
  return temp1;
};
