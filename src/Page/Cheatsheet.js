"use strict";

var Remarkable = require('remarkable');
var hljs       = require('highlightjs'); // https://highlightjs.org/

// Actual default values
var md = new Remarkable({
  highlight: function (str, lang) {
    if (lang && hljs.getLanguage(lang)) {
      try {
        return hljs.highlight(lang, str).value;
      } catch (err) {}
    }

    try {
      return hljs.highlightAuto(str).value;
    } catch (err) {}

    return ''; // use external default escaping
  }
});

exports.markdown = md;

exports.markdownRender = function(text){
    return function(md){
        return function(){ 
            return md.render(text);
        }
    }
}