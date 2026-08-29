// PURPOSE: give an R argument name the colour the code theme already declares for it.
// ROLE: shipped as a pkgdown asset and pulled in by inst/pkgdown/BS5/templates/in-header.html, so a
//   consumer's _pkgdown.yml stays one line. Hand-written; there is no palette in it.
//
// WHY IT EXISTS: an argument name is tagged by one highlighter and not the other. Pandoc marks
//   `pct =` as Attribute (<span class="at">pct =</span>), so Quarto colours it from the .theme file
//   for free; downlit leaves the NAME as bare text -- only the `=` beside it is tagged -- and no CSS
//   selector can reach a bare text node. So the name is wrapped here, and one `.at` colour then
//   serves both toolchains.
//
// TWO THINGS MAKE IT EXACT RATHER THAN A HEURISTIC, and neither is a guess:
//   - it works on the PARSED DOM, so it can never touch a string or a comment;
//   - downlit emits each operator as one span, so the `=== "="` test can never match `==`, `<=`,
//     `!=` or `<-`.
// A PANDOC block is immune by construction: there the `=` lives INSIDE the `at` span, so the loop
// finds no `op` span to act on. No block-level guard is needed, and one would be wrong -- it would
// skip any block carrying an `at` token for another reason.
document.querySelectorAll("pre code").forEach(code => {
  code.querySelectorAll("span.op").forEach(op => {
    if (op.textContent !== "=") return;
    const prev = op.previousSibling;
    if (!prev || prev.nodeType !== 3) return;
    const m = prev.textContent.match(/([A-Za-z.][\w.]*)\s*$/);
    if (!m) return;
    const at = document.createElement("span");
    at.className = "at";
    at.textContent = m[1];
    prev.textContent = prev.textContent.slice(0, m.index);
    op.parentNode.insertBefore(at, op);
  });
});
