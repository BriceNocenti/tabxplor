/* A GLOBAL OKLCH CHROMA CAP, applied to a whole page at once.
 *
 * Every colour the page states is converted to OKLCH, its chroma clamped to the cap, and converted
 * back. Lightness and hue are untouched, so nothing moves except how saturated it is -- which is the
 * point: it answers "how would this page feel with less colour in it?" without redesigning anything.
 *
 * WHY NOT `filter: saturate()`: that is a matrix in linear sRGB. It SCALES saturation instead of
 * capping it (so an already-calm colour is dulled as much as a loud one), it drags perceived
 * lightness with it, and a filter on an ancestor creates a containing block -- which breaks the
 * `position: sticky` sidebars both previews rely on.
 *
 * HOW: the text of every inline <style> is snapshotted once, and each cap change rewrites the
 * snapshot -- never the previous result, or the caps would compound.
 *
 * ⚠ THE ONE LIMITATION, stated plainly: a LINKED stylesheet cannot be read from a file:// page
 * (its rules are cross-origin, and fetch() is blocked). On the site preview that is pkgdown's
 * compiled bootstrap.min.css, so the page chrome it defines -- background, borders, its own greys --
 * keeps its colour. Those sit at chroma ~0.01 and a cap would not move them anyway; everything the
 * previews themselves state (tabxplor's ladders, the annotations, the code themes, the headings,
 * the chrome overrides) is inline, and is capped.
 */
(function () {
  const M2i = [[1, 0.3963377774, 0.2158037573],
               [1, -0.1055613458, -0.0638541728],
               [1, -0.0894841775, -1.2914855480]];
  const M1i = [[4.0767416621, -3.3077115913, 0.2309699292],
               [-1.2684380046, 2.6097574011, -0.3413193965],
               [-0.0041960863, -0.7034186147, 1.7076147010]];
  const lin = c => c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
  const enc = c => c <= 0.0031308 ? 12.92 * c : 1.055 * Math.pow(c, 1 / 2.4) - 0.055;

  function hexToLab(hex) {
    const r = lin(parseInt(hex.slice(1, 3), 16) / 255),
          g = lin(parseInt(hex.slice(3, 5), 16) / 255),
          b = lin(parseInt(hex.slice(5, 7), 16) / 255);
    const l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b),
          m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b),
          s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);
    return [0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
            1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
            0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s];
  }
  function labToHex(L, A, B) {
    const l = Math.pow(M2i[0][0] * L + M2i[0][1] * A + M2i[0][2] * B, 3),
          m = Math.pow(M2i[1][0] * L + M2i[1][1] * A + M2i[1][2] * B, 3),
          s = Math.pow(M2i[2][0] * L + M2i[2][1] * A + M2i[2][2] * B, 3);
    const v = [M1i[0][0] * l + M1i[0][1] * m + M1i[0][2] * s,
               M1i[1][0] * l + M1i[1][1] * m + M1i[1][2] * s,
               M1i[2][0] * l + M1i[2][1] * m + M1i[2][2] * s];
    return "#" + v.map(c => Math.round(Math.max(0, Math.min(1, enc(Math.max(0, Math.min(1, c))))) * 255)
                            .toString(16).padStart(2, "0")).join("");
  }
  // Lowering chroma at a fixed lightness and hue always moves INTO the gamut, so no gamut mapping is
  // needed here -- unlike raising it, which is why the R side has a binary search and this does not.
  function capHex(hex, cap) {
    const [L, A, B] = hexToLab(hex);
    const C = Math.hypot(A, B);
    if (C <= cap) return hex;
    const k = cap / C;
    return labToHex(L, A * k, B * k);
  }

  const HEX = /#[0-9a-fA-F]{6}\b/g;
  const RGB = /rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d.]+)\s*)?\)/g;
  function capText(css, cap) {
    return css
      .replace(HEX, h => capHex(h, cap))
      .replace(RGB, (m, r, g, b, a) => {
        const hex = "#" + [r, g, b].map(x => (+x).toString(16).padStart(2, "0")).join("");
        const out = capHex(hex, cap);
        const c = [1, 3, 5].map(i => parseInt(out.slice(i, i + 2), 16));
        return a === undefined ? `rgb(${c.join(",")})` : `rgba(${c.join(",")},${a})`;
      });
  }

  // The snapshot is the UNCAPPED text of each style element, and every cap is computed from it --
  // never from the previous result, or two changes in a row would compound.
  // ⚠ A layer the page rewrites at runtime (the chrome overrides, the heading rules) must hand its
  // new text to setLive() rather than assign it: that is what keeps its snapshot uncapped.
  const snaps = new Map();
  let current = null;

  function snapshot() {
    document.querySelectorAll("style").forEach(el => {
      if (!snaps.has(el)) snaps.set(el, el.textContent);
    });
  }
  function paint() {
    snaps.forEach((css, el) => { el.textContent = current === null ? css : capText(css, current); });
  }
  function set(cap) { current = cap; snapshot(); paint(); }
  function setLive(el, css) {
    snaps.set(el, css);
    el.textContent = current === null ? css : capText(css, current);
  }

  window.txChromaCap = { set, setLive, snapshot, get current() { return current; } };
})();
