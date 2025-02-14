var {x, y} = {x: 5, y: 10};
var p = {x, y};
var foo = (temp0) => {
  var {x, y} = temp0;
  return x + y;
};
var sum = foo({x: 5, y: 10});
foo({x, y});
