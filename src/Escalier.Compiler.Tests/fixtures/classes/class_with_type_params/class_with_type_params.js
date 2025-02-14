var Foo = class {
  bar
  map(temp0) {
    var callback = temp0;
    return callback(self.bar);
  }
};
var foo = new Foo();
