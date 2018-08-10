"use strict";

exports.requestAnimationFrame = function (fn) {
    return function () {
        return window.requestAnimationFrame(fn);
    }
}