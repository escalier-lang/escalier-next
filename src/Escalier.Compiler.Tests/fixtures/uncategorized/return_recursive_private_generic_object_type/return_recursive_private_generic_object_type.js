var makeTree = function () {
  var node = {value: 5, left: {value: 10}, right: {value: 15}};
  return node;
};
var node = makeTree();
var x = node.left?.value;
