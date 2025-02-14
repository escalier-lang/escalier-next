import {jsx as _jsx} from "react/jsx-runtime"
var Comps = {Comp: function (temp0) {
  var props = temp0;
  return _jsx("div", {children: [_jsx("p", {children: [props.message]})]});
}};
var comp = _jsx(Comps.Comp, {message: "Hello, world!"});
