var even = (x) => {
  var temp0;
  if (x == 0) {
    temp0 = true;
  } else {
    temp0 = !odd((x - 1));
  }
  return temp0;
};
var odd = (x) => {
  var temp1;
  if (x == 1) {
    temp1 = true;
  } else {
    temp1 = !even((x - 1));
  }
  return temp1;
};
