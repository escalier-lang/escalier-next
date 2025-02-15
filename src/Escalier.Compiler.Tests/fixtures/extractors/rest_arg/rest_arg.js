class C {
  first
  second
  third
  constructor(temp0, temp1, temp2) {
    var first = temp0;
    var second = temp1;
    var third = temp2;
    self.first = first;
    self.second = second;
    self.third = third;
  }
  [Symbol.customMatcher]() {
    return [self.first, self.second, self.third];
  }
}
var subject = new C("hello", 5, true);
const [temp3, ...temp4] = InvokeCustomMatcherOrThrow(C, subject, undefined);
var x = temp3;
var y = temp4;
