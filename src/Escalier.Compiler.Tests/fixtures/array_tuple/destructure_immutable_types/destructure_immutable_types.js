import {Record, Tuple} from "@bloomberg/record-tuple-polyfill"
var [a, b] = Tuple([5, "hello"]);
var [c, d] = Tuple([5, "hello"]);
var {e, f} = Record({e: 5, f: "hello"});
var {g, h} = Record({g: 5, h: "hello"});
