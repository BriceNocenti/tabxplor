/* tabxplor: bind the bootstrap tooltip and popover plugins to the attributes the html engine
   already writes (data-toggle="tooltip" / "popover", built by hand in R/tab-render-html.R).
   Without this the title= attribute still shows a plain browser tooltip; this upgrades it to the
   bootstrap one and is what makes popovers work at all.

   Adapted from kePrint.js of the kableExtra package (Hao Zhu, MIT licence), with thanks. */
$(document).ready(function () {
  var $t = $('[data-toggle="tooltip"]');
  if (typeof $t.tooltip === 'function') { $t.tooltip(); }
  var $p = $('[data-toggle="popover"]');
  if (typeof $p.popover === 'function') { $p.popover(); }
});
