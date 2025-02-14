function isEven(temp0) {
  var n = temp0;
  var temp1;
  if (n == 0) {
    temp1 = true;
  } else {
    temp1 = isOdd((n - 1));
  }
  return temp1;
}
function isOdd(temp2) {
  var n = temp2;
  var temp3;
  if (n == 0) {
    temp3 = false;
  } else {
    temp3 = isEven((n - 1));
  }
  return temp3;
}
