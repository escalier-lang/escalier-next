var foo = (x, y) => {
  var temp0;
  if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y} && {x, y}["y"] == 0) {
    temp0 = "origin";
  } else {
    var temp1;
    if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y} && {x, y}["y"] == 0) {
      temp1 = "x-axis";
    } else {
      var temp2;
      if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y}) {
        temp2 = "y-axis";
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
var bar = (x, y) => {
  var temp4;
  if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y} && {x, y}["y"] == 0) {
    temp4 = "origin";
  } else {
    var temp5;
    if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y} && {x, y}["y"] == 0) {
      temp5 = "x-axis";
    } else {
      var temp6;
      if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y}) {
        temp6 = "y-axis";
      } else {
        var temp7;
        if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y}) {
          temp7 = "other";
        }
        temp6 = temp7;
      }
      temp5 = temp6;
    }
    temp4 = temp5;
  }
  return temp4;
};
