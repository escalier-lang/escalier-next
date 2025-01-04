var foo = async function () {
  return 5;
};
var bar = async function () {
  var x = await foo();
  return x + await 10;
};
