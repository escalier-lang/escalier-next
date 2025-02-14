var C = class {
  first
  second
  constructor(first, second) {
    self.first = first;
    self.second = second;
  }
  [Symbol.customMatcher]() {
    return [self.first, self.second];
  }
};
var subject = new C("hello", 5);
const [x, y] = InvokeCustomMatcherOrThrow(C, subject, undefined);
