/* tabxplor: bind the tooltip and popover plugins to the attributes the html engine already writes
   (data-toggle="tooltip" / "popover", built by hand in R/tab-render-html.R).

   TWO BINDINGS, because there are two Bootstraps. Bootstrap 5 dropped the jQuery plugin API and
   exposes a `bootstrap` global instead (its bundle carries Popper); Bootstrap 3/4, which
   rmarkdown ships, is a jQuery plugin. Without the first branch a page served with Bootstrap 5 --
   a pkgdown site, a bslib document -- fell back to the browser's own `title=` tooltip, which
   appears only after about a second.

   The delay is stated explicitly on both paths: a tooltip on a table cell is read by hovering
   along a row, so waiting for it defeats the point.

   Adapted from kePrint.js of the kableExtra package (Hao Zhu, MIT licence), with thanks. */
(function () {
  function bind() {
    var bs = window.bootstrap;
    var tips = document.querySelectorAll('[data-toggle="tooltip"]');
    var pops = document.querySelectorAll('[data-toggle="popover"]');
    var i;
    if (bs && bs.Tooltip) {
      for (i = 0; i < tips.length; i++) {
        new bs.Tooltip(tips[i], {placement: 'right', container: 'body', delay: 0});
      }
      if (bs.Popover) {
        for (i = 0; i < pops.length; i++) {
          new bs.Popover(pops[i], {placement: 'right', container: 'body',
                                   trigger: 'hover focus', delay: 0});
        }
      }
      return;
    }
    if (typeof window.jQuery === 'function') {
      var $ = window.jQuery;
      var $t = $(tips);
      if (typeof $t.tooltip === 'function') { $t.tooltip({delay: 0}); }
      var $p = $(pops);
      if (typeof $p.popover === 'function') { $p.popover({delay: 0}); }
    }
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bind);
  } else {
    bind();
  }
})();
