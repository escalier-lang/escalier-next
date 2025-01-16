var getAdd5 = function () {
  var y = 5;
  return function (x) {
    return x + y;
  };
};
var add5 = getAdd5();
