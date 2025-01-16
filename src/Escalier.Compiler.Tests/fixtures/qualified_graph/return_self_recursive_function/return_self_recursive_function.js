var ret_fact = function () {
  var fact = (n) => {
    var temp0;
    if (n == 0) {
      temp0 = 1;
    } else {
      temp0 = n * fact((n - 1));
    }
    return temp0;
  };
  return fact;
};
