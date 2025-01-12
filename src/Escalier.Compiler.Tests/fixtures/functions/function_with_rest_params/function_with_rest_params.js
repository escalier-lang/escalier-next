var foo = (a, ...b) => {
  return a;
};
var a = foo(5);
var b = foo(10, "hello");
var c = foo(10, "hello", "world");
