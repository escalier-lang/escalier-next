var fact = (temp0) => {
  var n = temp0;
  var temp1;
  if (n == 0) {
    temp1 = 1;
  } else {
    temp1 = n * fact((n - 1));
  }
  return temp1;
};
var fib = (temp2) => {
  var n = temp2;
  var temp3;
  if (n <= 1) {
    temp3 = n;
  } else {
    temp3 = fib((n - 1)) + fib((n - 2));
  }
  return temp3;
};
