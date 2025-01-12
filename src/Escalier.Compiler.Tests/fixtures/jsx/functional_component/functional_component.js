import {jsx as _jsx} from "react/jsx-runtime"
var Comp = function (props) {
  return _jsx("div", {children: [_jsx("p", {children: [props.message]})]});
};
var comp = _jsx(Comp, {message: "Hello, world!"});
