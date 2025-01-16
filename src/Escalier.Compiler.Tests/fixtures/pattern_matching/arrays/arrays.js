var sum = (array) => {
  var temp0;
  if (array.length == 0) {
    temp0 = 0;
  } else {
    var temp1;
    if (array.length == 1) {
      temp1 = x;
    } else {
      var temp2;
      if (array.length == 2) {
        temp2 = x + y;
      } else {
        var temp3;
        if (array.length == 3) {
          temp3 = x + y + z;
        } else {
          var temp4;
          if (array.length == 4) {
            temp4 = x + y + z + sum(rest);
          }
          temp3 = temp4;
        }
        temp2 = temp3;
      }
      temp1 = temp2;
    }
    temp0 = temp1;
  }
  return temp0;
};
