var foo = (temp0, temp1) => {
  var x = temp0;
  var y = temp1;
  var temp2;
  if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y} && {x, y}["y"] == 0) {
    var {} = {x, y};
    temp2 = "origin";
  } else {
    var temp3;
    if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y} && {x, y}["y"] == 0) {
      var {x} = {x, y};
      temp3 = "x-axis";
    } else {
      var temp4;
      if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y}) {
        var {y} = {x, y};
        temp4 = "y-axis";
      } else {
        var temp5;
        if (true) {
          var _ = {x, y};
          temp5 = "other";
        }
        temp4 = temp5;
      }
      temp3 = temp4;
    }
    temp2 = temp3;
  }
  return temp2;
};
var bar = (temp6, temp7) => {
  var x = temp6;
  var y = temp7;
  var temp8;
  if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y} && {x, y}["y"] == 0) {
    var {} = {x, y};
    temp8 = "origin";
  } else {
    var temp9;
    if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y} && {x, y}["y"] == 0) {
      var {x} = {x, y};
      temp9 = "x-axis";
    } else {
      var temp10;
      if (typeof {x, y} == "object" && "x" in {x, y} && {x, y}["x"] == 0 && "y" in {x, y}) {
        var {y} = {x, y};
        temp10 = "y-axis";
      } else {
        var temp11;
        if (typeof {x, y} == "object" && "x" in {x, y} && "y" in {x, y}) {
          var {x, y} = {x, y};
          temp11 = "other";
        }
        temp10 = temp11;
      }
      temp9 = temp10;
    }
    temp8 = temp9;
  }
  return temp8;
};
