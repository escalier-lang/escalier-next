class C {
  msg
  constructor(temp0) {
    var value = temp0;
    self.msg = value;
  }
  [Symbol.customMatcher]() {
    return [self.msg];
  }
}
var subject = new C("hello");
const [temp1] = InvokeCustomMatcherOrThrow(C, subject, undefined);
var msg = temp1;
