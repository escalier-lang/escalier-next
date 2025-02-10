var C = class {
  data
  constructor(data) {
    self.data = data;
  }
  [Symbol.customMatcher]() {
    return [self.data];
  }
};
var subject = new C({x: 1, y: 2});
const [{x, y}] = InvokeCustomMatcherOrThrow(C, subject, undefined);
