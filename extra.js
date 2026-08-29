// Site-specific JS, and only that. pkgdown links it from <head>, right after its own
// lightswitch.js and before the page paints -- so what this changes is never seen changing.
//
// WHY: the light switch stays, but the site opens LIGHT whatever the reader's OS is set to.
// pkgdown's default is `prefers-color-scheme`; here dark is opt-in, because a colour-coded table is
// read on a white page by convention and the dark ramps keep less of their separation than the
// light ones do. A reader who wants dark clicks it once, and that choice is then honoured for good.
//
// HOW: seed the very key pkgdown's switch reads. Setting the attribute alone would leave the switch
// claiming "Auto" over a light page, and the OS-change listener would flip it back on the next
// system change; a stored value makes the button, the page and the listener say the same thing.
// Anything the reader has already chosen -- light, dark OR auto -- is left untouched.
(function () {
  try {
    if (localStorage.getItem('theme')) return;
    localStorage.setItem('theme', 'light');
  } catch (e) {
    // storage blocked (private window, cookies off): the attribute below is still the right default
  }
  document.documentElement.setAttribute('data-bs-theme', 'light');
})();
