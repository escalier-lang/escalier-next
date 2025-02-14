var foo = (temp0) => {
  var x = temp0;
  var temp1;
  if (x == 0) {
    temp1 = "none";
  } else {
    var temp2;
    if (x == 1) {
      temp2 = "one";
    } else {
      var temp3;
      if (true) {
        var n = x;
        temp3 = "negative";
      } else {
        var temp4;
        if (true) {
          var _ = x;
          temp4 = "other";
        }
        temp3 = temp4;
      }
      temp2 = temp3;
    }
    temp1 = temp2;
  }
  return temp1;
};
