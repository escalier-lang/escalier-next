var ret_fact = function () {
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
  return fact;
};
