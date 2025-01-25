var {x, y} = {x: 5, y: 10};
var p = {x, y};
var foo = ({x, y}) => {
  return x + y;
};
var sum = foo({x: 5, y: 10});
foo({x, y});
