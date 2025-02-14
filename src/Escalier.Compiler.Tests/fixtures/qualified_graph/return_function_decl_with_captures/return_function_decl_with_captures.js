var getAdd5 = function () {
  var y = 5;
  return function (temp0) {
    var x = temp0;
    return x + y;
  };
};
var add5 = getAdd5();
