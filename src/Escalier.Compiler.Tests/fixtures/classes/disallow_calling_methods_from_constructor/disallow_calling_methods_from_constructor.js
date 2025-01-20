var Foo = class {
msg
constructor(msg) {
  self.msg = self.bar();
}
bar() {
  return self.msg;
}
};
var foo = new Foo("hello");
