class C {
  first
  second
  constructor(temp0, temp1) {
    var first = temp0;
    var second = temp1;
    self.first = first;
    self.second = second;
  }
  [Symbol.customMatcher]() {
    return [self.first, self.second];
  }
}
var subject = new C("hello", 5);
const [x, y] = InvokeCustomMatcherOrThrow(C, subject, undefined);
