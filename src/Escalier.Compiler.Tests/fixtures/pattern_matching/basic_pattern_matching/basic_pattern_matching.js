var foo = (x) => {
  var temp0;
  if (x == 0) {
    temp0 = "none";
  } else {
    var temp1;
    if (x == 1) {
      temp1 = "one";
    } else {
      var temp2;
      if (true) {
        temp2 = "negative";
      } else {
        var temp3;
        if (true) {
          temp3 = "other";
        }
        temp2 = temp3;
      }
      temp1 = temp2;
    }
    temp0 = temp1;
  }
  return temp0;
};
