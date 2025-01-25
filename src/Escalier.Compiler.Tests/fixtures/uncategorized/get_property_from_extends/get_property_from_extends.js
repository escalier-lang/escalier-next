var foo = async function () {
  var res = await fetch("https://google.com");
  return res.json();
};
var bar = foo();
