Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main";
   var f = function (t) {
      var y = t.y;
      var x = t.x;
      return t;
   };
   var Thing = F2(function (a,b) {
      return {_: {},x: a,y: b};
   });
   _elm.Main.values = {_op: _op
                      ,Thing: Thing
                      ,f: f};
   return _elm.Main.values;
};