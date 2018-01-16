// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function make(x) {
  return /* Head */[
          x,
          /* Empty */0
        ];
}

function prepend(x, y) {
  return /* Head */[
          x,
          y
        ];
}

function length(ll) {
  var _a = ll;
  var _i = 0;
  while(true) {
    var i = _i;
    var a = _a;
    if (a) {
      _i = i + 1 | 0;
      _a = a[1];
      continue ;
      
    } else {
      return i;
    }
  };
}

function hd(y) {
  if (y) {
    return /* Some */[y[0]];
  } else {
    return /* None */0;
  }
}

function tl(_y) {
  while(true) {
    var y = _y;
    if (y) {
      var y$1 = y[1];
      if (y$1) {
        _y = y$1;
        continue ;
        
      } else {
        return /* Head */[
                y[0],
                /* Empty */0
              ];
      }
    } else {
      return /* Empty */0;
    }
  };
}

function rev(ll) {
  var _ll1_ = ll;
  var _ll2_ = /* Empty */0;
  while(true) {
    var ll2_ = _ll2_;
    var ll1_ = _ll1_;
    if (ll1_) {
      _ll2_ = /* Head */[
        ll1_[0],
        ll2_
      ];
      _ll1_ = ll1_[1];
      continue ;
      
    } else {
      return ll2_;
    }
  };
}

function map(f, ll) {
  var map_ = function (f, _ll1_, _ll2_) {
    while(true) {
      var ll2_ = _ll2_;
      var ll1_ = _ll1_;
      if (ll1_) {
        _ll2_ = /* Head */[
          Curry._1(f, ll1_[0]),
          ll2_
        ];
        _ll1_ = ll1_[1];
        continue ;
        
      } else {
        return ll2_;
      }
    };
  };
  return rev(map_(f, ll, /* Empty */0));
}

exports.make    = make;
exports.prepend = prepend;
exports.length  = length;
exports.hd      = hd;
exports.tl      = tl;
exports.rev     = rev;
exports.map     = map;
/* No side effect */