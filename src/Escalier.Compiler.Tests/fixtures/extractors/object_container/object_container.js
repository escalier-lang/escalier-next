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
var subject = {foo: new C("hello")};
const [temp1] = InvokeCustomMatcherOrThrow(C, subject["foo"], undefined);
var msg = temp1;
var {} = subject;
