var factorial = (n) => {
  var temp0;
  if (n == 0) {
    temp0 = 1;
  } else {
    temp0 = n * factorial((n - 1));
  }
  return temp0;
};
