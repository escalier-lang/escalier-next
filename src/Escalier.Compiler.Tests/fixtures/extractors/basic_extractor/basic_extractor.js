var C = class {
  msg
  constructor(value) {
    self.msg = value;
  }
  [Symbol.customMatcher]() {
    return [self.msg];
  }
};
var subject = new C("hello");
const [msg] = InvokeCustomMatcherOrThrow(C, subject, undefined);
