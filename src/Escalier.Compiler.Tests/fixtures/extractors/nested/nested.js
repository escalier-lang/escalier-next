class D {
  data
  constructor(temp0) {
    var data = temp0;
    self.data = data;
  }
  [Symbol.customMatcher]() {
    return [self.data];
  }
}
class E {
  data
  constructor(temp1) {
    var data = temp1;
    self.data = data;
  }
  [Symbol.customMatcher]() {
    return [self.data];
  }
}
class C {
  data1
  data2
  constructor(temp2, temp3) {
    var data1 = temp2;
    var data2 = temp3;
    self.data1 = data1;
    self.data2 = data2;
  }
  [Symbol.customMatcher]() {
    return [self.data1, self.data2];
  }
}
var subject = new C(new D("hello"), new E({x: 5, y: 10}));
const [temp4, temp5] = InvokeCustomMatcherOrThrow(C, subject, undefined);
const [temp6] = InvokeCustomMatcherOrThrow(D, temp4, undefined);
var msg = temp6;
const [temp7] = InvokeCustomMatcherOrThrow(E, temp5, undefined);
var {x, y} = temp7;
