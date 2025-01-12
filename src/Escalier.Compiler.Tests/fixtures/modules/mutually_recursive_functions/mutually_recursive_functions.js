var foo = () => {
  return bar() + 1;
};
var bar = () => {
  return foo() - 1;
};
