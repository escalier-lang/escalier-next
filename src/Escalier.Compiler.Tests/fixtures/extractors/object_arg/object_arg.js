class C {
  data
  constructor(temp0) {
    var data = temp0;
    self.data = data;
  }
  [Symbol.customMatcher]() {
    return [self.data];
  }
}
var subject = new C({x: 1, y: 2});
const [temp1] = InvokeCustomMatcherOrThrow(C, subject, undefined);
var {x, y} = temp1;
