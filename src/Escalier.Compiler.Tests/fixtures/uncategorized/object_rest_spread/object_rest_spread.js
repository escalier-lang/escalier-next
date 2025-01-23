var obj1 = {a: 5, b: "hello", c: true};
var {a, ...rest} = obj1;
var obj2 = {a, ...rest};
var foo = ({a, ...rest}) => {
  return a;
};
foo(obj2);
