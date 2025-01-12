import {jsx as _jsx} from "react/jsx-runtime"
var Comps = {Comp: function (props) {
  return _jsx("div", {children: [_jsx("p", {children: [props.message]})]});
}};
var comp = _jsx(Comps.Comp, {message: "Hello, world!"});
