function fact(n) {
  var temp0;
  if (n == 0) {
    temp0 = 1;
  } else {
    temp0 = n * fact((n - 1));
  }
  return temp0;
}
function fib(n) {
  var temp1;
  if (n <= 1) {
    temp1 = n;
  } else {
    temp1 = fib((n - 1)) + fib((n - 2));
  }
  return temp1;
}
