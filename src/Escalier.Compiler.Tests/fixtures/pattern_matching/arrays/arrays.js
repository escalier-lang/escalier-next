var sum = (temp0) => {
  var array = temp0;
  var temp1;
  if (array.length == 0) {
    var [] = array;
    temp1 = 0;
  } else {
    var temp2;
    if (array.length == 1) {
      var [x] = array;
      temp2 = x;
    } else {
      var temp3;
      if (array.length == 2) {
        var [x, y] = array;
        temp3 = x + y;
      } else {
        var temp4;
        if (array.length == 3) {
          var [x, y, z] = array;
          temp4 = x + y + z;
        } else {
          var temp5;
          if (array.length == 4) {
            var [x, y, z, ...rest] = array;
            temp5 = x + y + z + sum(rest);
          }
          temp4 = temp5;
        }
        temp3 = temp4;
      }
      temp2 = temp3;
    }
    temp1 = temp2;
  }
  return temp1;
};
