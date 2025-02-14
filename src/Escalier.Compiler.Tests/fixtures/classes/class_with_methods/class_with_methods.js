var Foo = class {
  msg
  bar() {
    return self.msg;
  }
  baz(temp0) {
    var msg = temp0;
    self.msg = msg;
  }
};
var foo = new Foo();
