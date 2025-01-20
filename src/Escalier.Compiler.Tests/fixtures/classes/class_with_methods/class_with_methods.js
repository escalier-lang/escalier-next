var Foo = class {
  msg
  bar() {
    return self.msg;
  }
  baz(msg) {
    self.msg = msg;
  }
};
var foo = new Foo();
