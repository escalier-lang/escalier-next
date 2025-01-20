function add(__arg0__, __arg1__) {
  if (typeof __arg0__ == "number" && typeof __arg1__ == "number") {
    return __arg0__ + __arg1__;
  } else if (typeof __arg0__ == "string" && typeof __arg1__ == "string") {
    return __arg0__ + __arg1__;
  } else {
    throw new TypeError();
  }
}
var sum = add(5, 10);
var msg = add("hello, ", "world");
