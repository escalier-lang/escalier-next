var even = (temp0) => {
  var x = temp0;
  var temp1;
  if (x == 0) {
    temp1 = true;
  } else {
    temp1 = !odd((x - 1));
  }
  return temp1;
};
var odd = (temp2) => {
  var x = temp2;
  var temp3;
  if (x == 1) {
    temp3 = true;
  } else {
    temp3 = !even((x - 1));
  }
  return temp3;
};
