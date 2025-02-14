var Foo = class {
  msg
  constructor(temp0) {
    var msg = temp0;
    self.msg = self.bar();
  }
  bar() {
    return self.msg;
  }
};
var foo = new Foo("hello");
