var foo = (temp0) => {
  var x = temp0;
  var temp1;
  if (x < 0) {
    temp1 = Escalier.throw("RangeError");
  } else {
    temp1 = x;
  }
  return temp1;
};
var bar = async function (temp2) {
  var x = temp2;
  var y = await foo(x);
  return y + await 10;
};
