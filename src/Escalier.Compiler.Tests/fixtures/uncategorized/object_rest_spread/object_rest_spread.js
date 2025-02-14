var obj1 = {a: 5, b: "hello", c: true};
var {a, ...rest} = obj1;
var obj2 = {a, ...rest};
var foo = (temp0) => {
  var {a, ...rest} = temp0;
  return a;
};
foo(obj2);
