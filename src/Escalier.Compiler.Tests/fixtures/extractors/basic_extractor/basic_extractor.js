var C = class {
  msg
  constructor(temp0) {
    var value = temp0;
    self.msg = value;
  }
  [Symbol.customMatcher]() {
    return [self.msg];
  }
};
var subject = new C("hello");
const [msg] = InvokeCustomMatcherOrThrow(C, subject, undefined);
