import {jsx as _jsx} from "react/jsx-runtime"
var foo = _jsx("div", {onClick: function (event) {
  var x = event.clientX;
  var y = event.clientY;
  var slope = y / x;
}});
