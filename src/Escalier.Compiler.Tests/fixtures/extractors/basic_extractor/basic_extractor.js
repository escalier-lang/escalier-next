var Foo = class {
  msg
  constructor(msg) {
    self.msg = msg;
  }
  [Symbol.customMatch]() {
    return [self.msg];
  }
};
var foo = new Foo("hello");
const [msg] = InvokeCustomMatcherOrThrow(Foo, foo, undefined);
