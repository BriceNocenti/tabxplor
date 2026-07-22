// Custom UI events for the jmvtabreg (Regressions) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep it
// lean. jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

// The file extension shown after the file name on the path line -- follows the chosen format. Rendered
// into the tiny `extCtrl` CustomControl (.u.yaml) on create, on format change, and on each onUpdate.
// The Export button keeps its static "Export" label (set in .u.yaml) -- no dynamic rename, no fixed width.
var exportExts = { excel: ".xlsx", html: ".html", md: ".md" };
var renderExt = function(ui) {
    if (!ui.extCtrl || !ui.extCtrl.$el || !ui.extCtrl.$el[0]) return;
    var fmt  = ui.export_format ? ui.export_format.value() : "excel";
    var root = ui.extCtrl.$el[0];
    root.textContent = exportExts[fmt] || ".xlsx";
    root.style.cssText = "color:#555;white-space:nowrap;padding:0 2px;";
};

// Make the "default path" reset discreet: a small, underlined, link-like secondary action rather than a
// primary button. Re-applied on each onUpdate (jamovi may re-render the control and drop inline styles).
var styleResetBtn = function(ui) {
    var c = ui.resetPath;
    if (!c || !c.$el || !c.$el[0]) return;
    var btn = c.$el[0].querySelector("button") || c.$el[0];
    btn.style.cssText += ";background:transparent;border:none;box-shadow:none;color:#777;" +
                         "font-size:11px;padding:1px 2px;min-width:0;width:auto;" +
                         "text-decoration:underline;cursor:pointer;";
};

// Phase 15c: the `subtext` note is a full-width, auto-grow <textarea> (a CustomControl driving the
// hidden `subtext` String option). Built once; re-syncs the option value into the textarea only when
// they diverge (e.g. reset / load). setValue is on `change` (blur), not per keystroke.
var autoGrowSubtext = function(ta) {
    ta.style.height = "auto";
    ta.style.height = (ta.scrollHeight + 2) + "px";
};
var renderSubtext = function(ui) {
    if (!ui.subtextCtrl || !ui.subtextCtrl.$el || !ui.subtextCtrl.$el[0]) return;
    var root = ui.subtextCtrl.$el[0];
    var val  = ui.subtext ? String(ui.subtext.value() || "") : "";
    var ta   = root.querySelector("textarea[data-tabx-subtext]");
    if (!ta) {
        root.innerHTML = "";
        ta = document.createElement("textarea");
        ta.setAttribute("data-tabx-subtext", "1");
        ta.rows = 3;
        ta.style.cssText = "width:100%;box-sizing:border-box;resize:vertical;min-height:3.6em;" +
                           "font-family:inherit;font-size:inherit;padding:4px 6px;" +
                           "border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;";
        ta.value = val;
        ta.addEventListener("input",  function() { autoGrowSubtext(ta); });
        ta.addEventListener("change", function() { if (ui.subtext) ui.subtext.setValue(ta.value); });
        root.appendChild(ta);
        autoGrowSubtext(ta);
    } else if (ta.value !== val) {
        ta.value = val;
        autoGrowSubtext(ta);
    }
};

// Grey the survey-design controls (cluster ids / strata / fpc / nest), which are only used with a
// weight. `wt` is a Variables slot, so its emptiness cannot be expressed in the declarative `enable:`
// DSL -> imperative setEnabled, re-run from onUpdate + onChange_vars. Values are preserved (the
// backend ignores them without a weight anyway).
var applyWtEnables = function(ui) {
    var w = ui.wt ? ui.wt.value() : null;
    var hasWt = !!(w && w.length > 0);
    ["ids", "strata", "fpc", "nest"].forEach(function(nm) {
        if (ui[nm] && ui[nm].setEnabled) ui[nm].setEnabled(hasWt);
    });
};

// WHY the path boxes ignored their stretch: jamovi compiles every grid cell to
// `minmax(max-content, <stretch>fr)`, so a control claims its min-content width BEFORE the stretch
// factors divide the row. A `width: largest` TextBox has a ~200px min (class silky-option-largest-text),
// so both boxes bottom out at ~200px. Collapse that floor with a persistent <style> (inline styles are
// dropped on jamovi re-renders). Folder keeps a 260px minimum so it stays the wider box (tune to taste);
// the file name (width: large) collapses fully. Only the two export boxes use these width classes.
// Phase h: also spaces the options collapse boxes apart (breathing room below each pane). The collapse-
// box. Last Phase r: the empty line at the BOTTOM of each collapse box (blank only while EXPANDED, so a
// collapsed pane stays compact and reads clearly apart from the next). The live jamovi collapse box is
// `.jmv-collapse-view` and its collapsed state is `.view-colapsed` (jamovi's spelling) -- confirmed
// against dev/jamovi/dev_console_live_capture/.../analysisui-*.css. The former guessed classes
// (.silky-options-collapse-box*, .jmv-options-collapsebox*, .silky-layout-content) matched NOTHING in
// the DOM, which is why earlier attempts never showed. `padding-bottom` sits inside the box border;
// `:not(.view-colapsed)` drops it when collapsed.
var injectTabxCss = function() {
    if (document.getElementById("tabx-css")) return;
    var s = document.createElement("style");
    s.id = "tabx-css";
    s.textContent =
        "input.silky-option-largest-text{min-width:260px !important;width:100% !important;box-sizing:border-box;}" +
        "input.silky-option-large-text{min-width:0 !important;width:100% !important;box-sizing:border-box;}" +
        ".jmv-collapse-view:not(.view-colapsed){padding-bottom:10px;}";
    document.head.appendChild(s);
};

// Phase o: the Export block sits OUTSIDE the collapse hierarchy. The former <hr> was a jamovi Label
// whose raw HTML jamovi escaped -> it showed as literal "<hr ...>" text. Draw the rule properly instead:
// walk up from the Export button to its `margin: large` block container (same ancestor bottomAlignInRow
// targets) and give it a border-top. Full-width, real rule, no raw text. Idempotent (a plain style set).
var styleExportSep = function(ui) {
    var c = ui.exportExcel;
    if (!c || !c.$el || !c.$el[0]) return;
    var node = c.$el[0], guard = 0;
    while (node && guard++ < 16) {
        if (node.classList && node.classList.contains("silky-control-margin-large")) {
            node.style.borderTop  = "1px solid rgba(0,0,0,0.18)";
            node.style.marginTop  = "10px";
            node.style.paddingTop = "8px";
            return;
        }
        node = node.parentElement;
    }
};

// Last Phase r: the "Run comparison" action matches the Export button -- jamovi's DEFAULT ActionButton
// look (theme-correct blue background, white bold text), so we DON'T recolour it (the Phase-o material
// grey is dropped). Only keep the blank line below it (the empty line the maintainer asked for at the
// bottom of the box). Re-applied each onUpdate because jamovi re-renders drop inline styles.
var styleRunCompareBtn = function(ui) {
    var c = ui.run_compare;
    if (!c || !c.$el || !c.$el[0]) return;
    var btn = c.$el[0].querySelector("button") || c.$el[0];
    btn.style.marginBottom = "8px";
};

// Push a control to the BOTTOM of its (taller) row: walk up to its row-item cell (the one whose grid is a
// direct child of the export block's `margin: large` container -- true regardless of any inner label
// wrapper) and set `align-self: flex-end`. Re-applied each onUpdate (re-renders drop inline styles).
var bottomAlignInRow = function(ui, name) {
    var c = ui[name];
    if (!c || !c.$el || !c.$el[0]) return;
    var node = c.$el[0], guard = 0;
    while (node && guard++ < 14) {
        if (node.classList && node.classList.contains("silky-layout-cell")) {
            var g = node.parentElement && node.parentElement.parentElement &&
                    node.parentElement.parentElement.parentElement;
            if (g && g.classList && g.classList.contains("silky-control-margin-large")) {
                node.style.alignSelf = "flex-end";
                return;
            }
        }
        node = node.parentElement;
    }
};

var onUpdate = function(ui) {
    injectTabxCss();
    applyWtEnables(ui);
    renderSubtext(ui);
    renderExt(ui);
    styleResetBtn(ui);
    styleExportSep(ui);                      // Phase o: thin rule above the (out-of-hierarchy) Export block
    styleRunCompareBtn(ui);                  // Last Phase r: default (Export-look) button + blank line
    bottomAlignInRow(ui, "export_format");   // Format combo -> bottom of row 1 (aligns with Export button)
    bottomAlignInRow(ui, "extCtrl");         // ".ext" text -> bottom of the path row
    renderModelTable(ui);
    renderRefPicker(ui);
    renderModelBuilder(ui);
    applyCompareEnables(ui);
};

// Phase 15d: a valid model-comparison test needs every model fit on the SAME cases. Choosing a
// comparison forces `na = "drop_all_models"` (the shared complete-case population); the user may switch
// back to per-model dropping afterwards (re-opt-in). Guarded setValue -> idempotent, no update loop.
var forceNaForCompare = function(ui) {
    if (!ui.compare || !ui.na) return;
    var c = ui.compare.value();
    if ((c === "baseline" || c === "sequential") && ui.na.value() !== "drop_all_models")
        ui.na.setValue("drop_all_models");
};

// ---- Reference-level picker CustomControl (refPickerCtrl) ---------------------------------
// One Material line per FACTOR predictor = a bold name + a native <select> over its levels (the
// baseline the model contrasts against, default = the first level). Stored by LABEL in the hidden
// `refLevels` option, read by jmvtab_reg_ref_vector() -> tab_reg(reference =). Numeric predictors have
// no reference (a note). A reference change is reparametrized live from a cached fit (no refit).

var TABX = {
    refRow:  "display:grid;grid-template-columns:120px 1fr;align-items:center;gap:8px;width:74%;min-width:360px;box-sizing:border-box;padding:5px 8px;margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    refName: "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    refSel:  "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    refNote: "opacity:0.6;font-style:italic;",
    hint:    "padding:8px;opacity:0.65;font-style:italic;",
    // model-builder cards (one per model) + the numeric-predictor scaling input
    cardBox:  "border:1px solid rgba(0,0,0,0.14);border-radius:5px;background:rgba(0,0,0,0.02);margin:6px;padding:6px 8px;width:100%;min-width:320px;box-sizing:border-box;",
    cardHead: "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
    cardBase: "display:inline-flex;align-items:center;gap:3px;font-size:0.85em;opacity:0.8;white-space:nowrap;cursor:pointer;",
    cardName: "flex:1 1 auto;min-width:0;box-sizing:border-box;padding:2px 6px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;font-weight:600;",
    cardDel:  "flex:0 0 auto;border:none;background:transparent;cursor:pointer;font-size:1.1em;line-height:1;color:rgba(0,0,0,0.55);padding:2px 6px;",
    cardVars: "display:flex;flex-wrap:wrap;gap:4px 14px;",
    cardChk:  "display:inline-flex;align-items:center;gap:3px;white-space:nowrap;cursor:pointer;",
    addBtn:   "margin:4px 6px 8px;padding:4px 12px;border:1px dashed rgba(0,0,0,0.35);border-radius:4px;background:rgba(0,0,0,0.03);color:#000;cursor:pointer;font-weight:600;",
    // Phase 15d: `white-space:nowrap` keeps "x [k] per unit (numeric)" on ONE line (it used to wrap).
    multWrap: "display:flex;align-items:center;gap:2px;min-width:0;white-space:nowrap;",
    multInp:  "width:70px;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;",
    // per-dependent Model table: [name] [family select] [modelled level / trials]. Phase h: full width
    // (3 columns spanning all the space to the right), a wider family column + a stretching col-3 so long
    // level labels stay readable.
    mtRow:   "display:grid;grid-template-columns:150px 210px 1fr;align-items:center;gap:10px;width:100%;min-width:0;box-sizing:border-box;padding:5px 8px;margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    mtSel:   "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    mtTrials:"width:90px;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;"
};

var levelsCache = {};     // var -> [labels] | null (numeric/no-levels) | FETCHING sentinel
var FETCHING = {};
var lastRefSig = null;    // ref-picker signature (predictors)

var refSig = function(ui) {
    var preds = utils.clone(ui.predictors.value(), []);
    return JSON.stringify([preds]);
};

var afterFetch = function(ui) {
    if (ui.refPickerCtrl && ui.refPickerCtrl.$el) renderRefPicker(ui);
};

// The stored reference for predictor `v` in refLevels ("" if not picked).
var refSelected = function(ui, v) {
    var arr = utils.clone(ui.refLevels.value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i].ref == null ? "" : String(arr[i].ref));
    return "";
};

// Set/replace predictor `v`'s reference entry in refLevels.
var writeRef = function(ui, v, refval) {
    var arr = utils.clone(ui.refLevels.value(), []);
    var found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, ref: refval }; found = true; break; }
    if (!found) arr.push({ var: v, ref: refval });
    ui.refLevels.setValue(arr);
};

// Drop refLevels entries whose var is no longer a predictor (guarded setValue -> no loop).
var reconcileRefLevels = function(ui, preds) {
    var cur = utils.clone(ui.refLevels.value(), []);
    var kept = [];
    for (var i = 0; i < cur.length; i++)
        if (preds.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.refLevels.setValue(kept);
};

// ---- Numeric-predictor scaling (multiplicator) ------------------------------------------
// Folded into the numeric rows of the reference picker: a numeric predictor has no reference
// level, so its row instead offers a "x k per unit" input (OR/beta per k units, e.g. per decade
// of age). Stored by var in the hidden `multiplicator` Array; read by jmvtab_reg_mult_vector()
// -> tab_reg(multiplicator =). Only numeric rows expose it, so only numeric predictors are ever
// written (tab_reg validates the names are numeric).
var multGet = function(ui) {
    return ui.multiplicator ? utils.clone(ui.multiplicator.value(), []) : [];
};

var multSelected = function(ui, v) {
    var arr = multGet(ui);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i].k == null ? "" : String(arr[i].k));
    return "";
};

var writeMult = function(ui, v, kval) {
    if (!ui.multiplicator) return;
    var arr = multGet(ui), kept = [];
    for (var i = 0; i < arr.length; i++) if (arr[i].var !== v) kept.push(arr[i]);
    if (kval != null && String(kval).length > 0) kept.push({ var: v, k: String(kval) });
    ui.multiplicator.setValue(kept);
};

var reconcileMult = function(ui, preds) {
    if (!ui.multiplicator) return;
    var cur = multGet(ui), kept = [];
    for (var i = 0; i < cur.length; i++)
        if (preds.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.multiplicator.setValue(kept);
};

var refLineControl = function(nameText, levels, selectedRef, onPick) {
    var row = document.createElement("div"); row.style.cssText = TABX.refRow;
    var lab = document.createElement("b"); lab.style.cssText = TABX.refName; lab.textContent = nameText;
    row.appendChild(lab);
    var sel = document.createElement("select"); sel.style.cssText = TABX.refSel;
    levels.forEach(function(l) {
        var o = document.createElement("option");
        o.value = l; o.textContent = l;
        if (l === selectedRef) o.selected = true;
        sel.appendChild(o);
    });
    sel.addEventListener("change", function() { onPick(sel.value); });
    row.appendChild(sel);
    return row;
};

var renderRefVarCard = function(ui, frag, v) {
    var cached = (v in levelsCache) ? levelsCache[v] : undefined;
    if (cached === FETCHING) cached = undefined;
    if (cached === undefined) {
        var ph = document.createElement("div"); ph.style.cssText = TABX.refRow;
        var b0 = document.createElement("b"); b0.style.cssText = TABX.refName; b0.textContent = v;
        var d0 = document.createElement("span"); d0.style.cssText = TABX.refNote; d0.textContent = "…";
        ph.appendChild(b0); ph.appendChild(d0);
        frag.appendChild(ph);
        if (!(v in levelsCache)) {
            levelsCache[v] = FETCHING;
            ui.refPickerCtrl.requestData("column",
                { columnName: v, properties: ["measureType", "levels"] })
                .then(function(col) {
                    levelsCache[v] = (!col || col.measureType === "continuous")
                        ? null : col.levels.map(function(l) { return l.label; });
                    afterFetch(ui);
                })
                .catch(function() { levelsCache[v] = null; afterFetch(ui); });
        }
        return;
    }
    if (cached === null) {   // numeric predictor: no reference level -> a "x k per unit" scaling input
        var row = document.createElement("div"); row.style.cssText = TABX.refRow;
        var b1 = document.createElement("b"); b1.style.cssText = TABX.refName; b1.textContent = v;
        row.appendChild(b1);
        var wrap = document.createElement("span"); wrap.style.cssText = TABX.multWrap;
        var pre = document.createElement("span"); pre.textContent = "× ";
        var inp = document.createElement("input");
        inp.type = "number"; inp.step = "any"; inp.style.cssText = TABX.multInp;
        inp.placeholder = "1"; inp.value = multSelected(ui, v);
        inp.addEventListener("change", function() { writeMult(ui, v, inp.value); });
        var suf = document.createElement("span"); suf.style.cssText = TABX.refNote;
        suf.textContent = " per unit (numeric)";
        wrap.appendChild(pre); wrap.appendChild(inp); wrap.appendChild(suf);
        row.appendChild(wrap);
        frag.appendChild(row);
        return;
    }
    if (cached.length === 0) return;
    var stored = refSelected(ui, v);
    var selRef = (stored && cached.indexOf(stored) >= 0) ? stored : cached[0];   // default = first level
    frag.appendChild(refLineControl(v, cached, selRef, function(r) { writeRef(ui, v, r); }));
};

var renderRefPicker = function(ui) {
    if (!ui.refPickerCtrl || !ui.refLevels || !ui.predictors) return;
    lastRefSig = refSig(ui);
    var preds = utils.clone(ui.predictors.value(), []);
    reconcileRefLevels(ui, preds);
    reconcileMult(ui, preds);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-refpick", "1");
    if (preds.length === 0) {
        var hint = document.createElement("div"); hint.style.cssText = TABX.hint;
        hint.textContent = "Select predictors to choose their reference (baseline) level.";
        frag.appendChild(hint);
    } else {
        preds.forEach(function(v) { renderRefVarCard(ui, frag, v); });
    }
    var root = ui.refPickerCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

// ---- Per-dependent Model table CustomControl (modelTableCtrl) ----------------------------
// One row per DEPENDENT = [name] [family select filtered by the outcome's R type] [col-3]. col-3 is
// the binomial MODELLED level (for a 2-level factor, default the FIRST level = the modelled/success
// level) or the number of TRIALS (for a numeric binomial outcome; blank -> the observed max). Stored in
// the hidden depFamily / depModelLevel / depTrials arrays, folded by jmvtab_reg_* into tab_reg(family /
// inverse_two_level_factors / trials). Phase h: the family is DETECTED client-side and stored as an
// explicit concrete pick (no "auto" default), so the backend never re-detects. Mirrors the refPicker's
// async column fetch (own cache: it needs measureType + dataType, which the refPicker's cache drops).

var mtCache = {};          // var -> {mt: measureType, dataType, levels: [labels]|null} | FETCHING
var lastModelSig = null;

// Phase h: no "auto (detected)" row -- the family is detected client-side and pre-selected as a
// CONCRETE choice; and no "quasipoisson" (an unweighted poisson already scales its SEs for
// over-dispersion, and it shortens the longest dropdown item). quasipoisson stays available in the R
// API only. Integer numeric outcomes default to poisson (counts), decimals to gaussian.
var FAMILY_LABEL = {
    gaussian: "gaussian (linear)", binomial: "binomial (logistic)", poisson: "poisson (counts)",
    multinomial: "multinomial (nominal)", ordinal: "ordinal (ordered)"
};

// The family detected from the outcome's R type (mirrors reg_detect_family, but resolves the integer
// count -> poisson that the R side leaves to an explicit pick). Stored explicitly so the backend never
// re-detects (and never aborts on an integer count).
var detectFamily = function(c) {
    if (!c || c.levels === null) return (c && c.dataType === "decimal") ? "gaussian" : "poisson";
    if (c.levels.length === 2) return "binomial";
    if (c.mt === "ordinal")    return "ordinal";
    return "multinomial";
};

var modelTableSig = function(ui) {
    var deps = utils.clone(ui.dependent.value(), []);
    var fams = ui.depFamily ? utils.clone(ui.depFamily.value(), []) : [];
    return JSON.stringify([deps, fams]);       // families included so col-3 re-renders on a family flip
};

var afterFetchMT = function(ui) {
    if (ui.modelTableCtrl && ui.modelTableCtrl.$el) renderModelTable(ui);
};

// families offered for an outcome's R type (numeric / 2-level factor / 3+ factor). Concrete only:
// a 2-level factor has a single option (binomial) -> the select is greyed out (nothing to choose).
var familyOptionsFor = function(c) {
    if (!c || c.levels === null) return ["gaussian", "binomial", "poisson"];
    if (c.levels.length === 2) return ["binomial"];
    if (c.mt === "ordinal")    return ["ordinal", "multinomial"];
    return ["multinomial", "ordinal"];
};

// stored per-dependent value from a {var, <key>} array ("" if unset).
var arrGet = function(ui, opt, v, key) {
    if (!ui[opt]) return "";
    var arr = utils.clone(ui[opt].value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i][key] == null ? "" : String(arr[i][key]));
    return "";
};

// set/replace a per-dependent {var, <key>:val} entry; a blank val removes it (-> backend default).
// Guarded (JSON compare) so an unchanged pick never re-fires `update`.
var arrWrite = function(ui, opt, v, key, val) {
    if (!ui[opt]) return;
    var arr = utils.clone(ui[opt].value(), []), kept = [], found = false, e;
    for (var i = 0; i < arr.length; i++) {
        if (arr[i].var !== v) { kept.push(arr[i]); continue; }
        found = true;
        if (val != null && String(val).length > 0) { e = { var: v }; e[key] = String(val); kept.push(e); }
    }
    if (!found && val != null && String(val).length > 0) { e = { var: v }; e[key] = String(val); kept.push(e); }
    if (JSON.stringify(arr) !== JSON.stringify(kept)) ui[opt].setValue(kept);
};

// drop entries whose var is no longer a dependent (guarded).
var reconcileArr = function(ui, opt, deps) {
    if (!ui[opt]) return;
    var cur = utils.clone(ui[opt].value(), []);
    var kept = cur.filter(function(e) { return deps.indexOf(e.var) >= 0; });
    if (kept.length !== cur.length) ui[opt].setValue(kept);
};

var makeSelect = function(style, options, labelOf, selected, onPick) {
    var sel = document.createElement("select"); sel.style.cssText = style;
    options.forEach(function(o) {
        var opt = document.createElement("option");
        opt.value = o; opt.textContent = labelOf ? (labelOf[o] || o) : o;
        if (o === selected) opt.selected = true;
        sel.appendChild(opt);
    });
    sel.addEventListener("change", function() { onPick(sel.value); });
    return sel;
};

var renderModelRow = function(ui, frag, v) {
    var c = (v in mtCache) ? mtCache[v] : undefined;
    if (c === FETCHING) c = undefined;
    if (c === undefined) {
        var ph = document.createElement("div"); ph.style.cssText = TABX.mtRow;
        var b0 = document.createElement("b"); b0.style.cssText = TABX.refName; b0.textContent = v;
        var d0 = document.createElement("span"); d0.style.cssText = TABX.refNote; d0.textContent = "…";
        ph.appendChild(b0); ph.appendChild(d0);
        frag.appendChild(ph);
        if (!(v in mtCache)) {
            mtCache[v] = FETCHING;
            ui.modelTableCtrl.requestData("column",
                { columnName: v, properties: ["measureType", "dataType", "levels"] })
                .then(function(col) {
                    mtCache[v] = (!col || col.measureType === "continuous")
                        ? { mt: "continuous", dataType: col ? col.dataType : "decimal", levels: null }
                        : { mt: col.measureType, dataType: col.dataType,
                            levels: col.levels.map(function(l) { return l.label; }) };
                    afterFetchMT(ui);
                })
                .catch(function() { mtCache[v] = { mt: "continuous", dataType: "decimal", levels: null };
                                    afterFetchMT(ui); });
        }
        return;
    }
    var row = document.createElement("div"); row.style.cssText = TABX.mtRow;
    var nm  = document.createElement("b"); nm.style.cssText = TABX.refName; nm.textContent = v;
    row.appendChild(nm);

    // Phase h: no "auto" -- pre-select the CONCRETE detected family and store it explicitly (so the
    // backend never re-detects / aborts on an integer count). A single option (2-level factor) is greyed.
    var opts     = familyOptionsFor(c);
    var detected = detectFamily(c);
    var storedF  = arrGet(ui, "depFamily", v, "family");
    var famSel   = (storedF && opts.indexOf(storedF) >= 0) ? storedF : detected;
    if (!storedF) arrWrite(ui, "depFamily", v, "family", detected);   // persist the detected default
    var famSelEl = makeSelect(TABX.mtSel, opts, FAMILY_LABEL, famSel,
        function(f) { arrWrite(ui, "depFamily", v, "family", f); renderModelTable(ui); });
    if (opts.length <= 1) famSelEl.disabled = true;
    row.appendChild(famSelEl);

    // col-3: a 2-level factor -> modelled-level picker; a numeric outcome set to binomial -> trials.
    var isBinFactor = c.levels && c.levels.length === 2;
    var isNumBinom  = (c.levels === null) && (famSel === "binomial");
    if (isBinFactor) {
        // Phase h: the level dropdown alone (no "model " label -- the user sees it lists the outcome's
        // levels, so it reads as the modelled-level picker) and it stretches to fill col-3.
        var storedL = arrGet(ui, "depModelLevel", v, "level");
        var selL = (storedL && c.levels.indexOf(storedL) >= 0) ? storedL : c.levels[0];  // default first
        row.appendChild(makeSelect(TABX.mtSel, c.levels, null, selL,
            function(l) { arrWrite(ui, "depModelLevel", v, "level", l === c.levels[0] ? "" : l); }));
    } else if (isNumBinom) {
        var wrapT = document.createElement("div"); wrapT.style.cssText = TABX.multWrap;
        var inp = document.createElement("input");
        inp.type = "number"; inp.step = "1"; inp.min = "1"; inp.style.cssText = TABX.mtTrials;
        inp.placeholder = "max"; inp.value = arrGet(ui, "depTrials", v, "n");
        inp.addEventListener("change", function() { arrWrite(ui, "depTrials", v, "n", inp.value); });
        var sufT = document.createElement("span"); sufT.style.cssText = TABX.refNote; sufT.textContent = " trials";
        wrapT.appendChild(inp); wrapT.appendChild(sufT);
        row.appendChild(wrapT);
    } else {
        row.appendChild(document.createElement("span"));       // keep the 3-column grid aligned
    }
    frag.appendChild(row);
};

var renderModelTable = function(ui) {
    if (!ui.modelTableCtrl || !ui.dependent) return;
    lastModelSig = modelTableSig(ui);
    var deps = utils.clone(ui.dependent.value(), []);
    reconcileArr(ui, "depFamily", deps);
    reconcileArr(ui, "depModelLevel", deps);
    reconcileArr(ui, "depTrials", deps);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-model", "1");
    if (deps.length === 0) {
        var hint = document.createElement("div"); hint.style.cssText = TABX.hint;
        hint.textContent = "Add one or more outcome variables to choose each one's model family.";
        frag.appendChild(hint);
    } else {
        deps.forEach(function(v) { renderModelRow(ui, frag, v); });
    }
    var root = ui.modelTableCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
    applyModelEnables(ui);
};

// Phase h: `effect` (AME) is meaningless for a pure-gaussian selection (AME == the coefficient), and
// `exponentiate` (odds/rate ratios) only bites on binomial / poisson / multinomial / ordinal. Both
// therefore enable exactly when SOME selected outcome is non-gaussian. The family is computed, not a
// declarative option, so grey imperatively (mirrors applyWtEnables); families come from mtCache, filled
// async -> re-run from renderModelTable / afterFetchMT as columns resolve.
var anyNonGaussian = function(ui) {
    if (!ui.dependent) return false;
    var deps = utils.clone(ui.dependent.value(), []);
    for (var i = 0; i < deps.length; i++) {
        var c = mtCache[deps[i]];
        if (!c || c === FETCHING) continue;
        var storedF = arrGet(ui, "depFamily", deps[i], "family");
        var fam = (storedF && FAMILY_LABEL[storedF]) ? storedF : detectFamily(c);
        if (fam !== "gaussian") return true;
    }
    return false;
};
var applyModelEnables = function(ui) {
    var on = anyNonGaussian(ui);
    ["effect_1", "effect_2", "exponentiate"].forEach(function(nm) {
        if (ui[nm] && ui[nm].setEnabled) ui[nm].setEnabled(on);
    });
};

// ---- Model-comparison builder CustomControl (modelBuilderCtrl) ---------------------------
// One card per model = an editable name + a checkbox per predictor in the pool (the `predictors`
// slot) + a delete button; a "+ Add model" button appends a card defaulting to the FULL pool.
// The cards are stored in the hidden `models` Array (Group{label, vars}); jmvtab_reg_models()
// folds them into tab_reg()'s `predictors` (an EMPTY builder -> the flat pool = single model; >=1
// card -> a named list = model comparison). When compare == "baseline", each card also shows a
// radio marker writing its 1-based position to the hidden `baseline` option.
//
// The signature deliberately EXCLUDES `models`/`baseline` (like refSig excludes refLevels): a
// checkbox / name / marker edit writes those and is SKIPPED by `updated`, so the in-place DOM edit
// stands; add / delete change the card COUNT and re-render synchronously in their own handlers.

var lastModelsSig = null;

var modelsSig = function(ui) {
    var pool    = utils.clone(ui.predictors.value(), []);
    var compare = ui.compare ? ui.compare.value() : "none";
    return JSON.stringify([pool, compare]);
};

var modelsGet = function(ui) { return utils.clone(ui.models.value(), []); };

// Store card `i`'s checked vars in POOL ORDER (drop anything not in the pool). Guarded setValue.
var setCardVars = function(ui, i, checkedSet, pool) {
    var arr = modelsGet(ui);
    if (!arr[i]) return;
    arr[i] = { label: arr[i].label || "", vars: pool.filter(function(v) { return checkedSet[v]; }) };
    ui.models.setValue(arr);
};

var setCardLabel = function(ui, i, label) {
    var arr = modelsGet(ui);
    if (!arr[i]) return;
    arr[i] = { label: label, vars: (arr[i].vars || []).slice() };
    ui.models.setValue(arr);
};

var addCard = function(ui, pool) {                       // a new card defaults to the full pool
    var arr = modelsGet(ui);
    arr.push({ label: "", vars: pool.slice() });
    ui.models.setValue(arr);
    renderModelBuilder(ui);                              // count changed -> synchronous re-render
};

var deleteCard = function(ui, i) {
    var arr = modelsGet(ui);
    arr.splice(i, 1);
    ui.models.setValue(arr);
    reconcileBaseline(ui);
    renderModelBuilder(ui);
};

// Drop vars no longer in the pool; keep the card + its name (a growing pool does NOT retro-add a
// new predictor to existing cards -- correct model-comparison semantics). Guarded setValue.
var reconcileModels = function(ui, pool) {
    var arr = modelsGet(ui), changed = false;
    for (var i = 0; i < arr.length; i++) {
        var vars = arr[i].vars || [];
        var kept = pool.filter(function(v) { return vars.indexOf(v) >= 0; });
        if (kept.length !== vars.length) { arr[i] = { label: arr[i].label || "", vars: kept }; changed = true; }
    }
    if (changed) ui.models.setValue(arr);
};

// Keep the stored baseline position within 1..n after add / delete.
var reconcileBaseline = function(ui) {
    if (!ui.baseline) return;
    var n = modelsGet(ui).length;
    var b = ui.baseline.value() || 1;
    var clamped = Math.min(Math.max(b, 1), Math.max(n, 1));
    if (clamped !== b) ui.baseline.setValue(clamped);
};

// `compare` needs >=2 models; the card COUNT is invisible to the declarative enable: DSL, so grey
// it imperatively (mirrors applyWtEnables).
var applyCompareEnables = function(ui) {
    var n = (ui.models ? utils.clone(ui.models.value(), []) : []).length;
    if (ui.compare && ui.compare.setEnabled) ui.compare.setEnabled(n >= 2);
};

var renderModelCard = function(ui, frag, card, i, pool, showBaseline, basePos) {
    var box  = document.createElement("div"); box.style.cssText = TABX.cardBox;
    var head = document.createElement("div"); head.style.cssText = TABX.cardHead;

    if (showBaseline) {
        var rl = document.createElement("label"); rl.style.cssText = TABX.cardBase;
        var radio = document.createElement("input");
        radio.type = "radio"; radio.name = "tabx-baseline"; radio.checked = (i + 1 === basePos);
        radio.addEventListener("change", function() { if (ui.baseline) ui.baseline.setValue(i + 1); });
        rl.appendChild(radio); rl.appendChild(document.createTextNode(" baseline"));
        head.appendChild(rl);
    }

    var name = document.createElement("input");
    name.type = "text"; name.style.cssText = TABX.cardName;
    name.placeholder = "model" + (i + 1); name.value = card.label || "";
    name.addEventListener("input", function() { setCardLabel(ui, i, name.value); });
    head.appendChild(name);

    var del = document.createElement("button");
    del.type = "button"; del.style.cssText = TABX.cardDel; del.textContent = "×";
    del.title = "Remove this model";
    del.addEventListener("click", function() { deleteCard(ui, i); });
    head.appendChild(del);
    box.appendChild(head);

    var vbox = document.createElement("div"); vbox.style.cssText = TABX.cardVars;
    var checks = [];
    var checkedNow = {};
    (card.vars || []).forEach(function(v) { checkedNow[v] = true; });
    pool.forEach(function(v) {
        var lab = document.createElement("label"); lab.style.cssText = TABX.cardChk;
        var cb  = document.createElement("input"); cb.type = "checkbox"; cb.checked = !!checkedNow[v];
        cb.addEventListener("change", function() {
            var set = {};
            checks.forEach(function(c) { if (c.cb.checked) set[c.v] = true; });
            if (Object.keys(set).length === 0) { cb.checked = true; return; }  // keep >=1 per card
            setCardVars(ui, i, set, pool);
        });
        checks.push({ v: v, cb: cb });
        lab.appendChild(cb); lab.appendChild(document.createTextNode(" " + v));
        vbox.appendChild(lab);
    });
    box.appendChild(vbox);
    frag.appendChild(box);
};

var renderModelBuilder = function(ui) {
    if (!ui.modelBuilderCtrl || !ui.models || !ui.predictors) return;
    lastModelsSig = modelsSig(ui);
    var pool = utils.clone(ui.predictors.value(), []);
    reconcileModels(ui, pool);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-models", "1");

    if (pool.length === 0) {
        var h0 = document.createElement("div"); h0.style.cssText = TABX.hint;
        h0.textContent = "Select predictors first: they form the pool each model draws from.";
        frag.appendChild(h0);
    } else {
        var cards   = modelsGet(ui);
        var compare = ui.compare ? ui.compare.value() : "none";
        var showBaseline = (compare === "baseline" && cards.length >= 2);
        var basePos = ui.baseline ? (ui.baseline.value() || 1) : 1;
        cards.forEach(function(card, i) {
            renderModelCard(ui, frag, card, i, pool, showBaseline, basePos);
        });
        var note = document.createElement("div"); note.style.cssText = TABX.hint;
        note.textContent = (cards.length === 0)
            ? "Add two or more models to compare specifications; leave empty to fit one model on all predictors."
            : "Each model draws from the predictors above; untick to leave a predictor out.";
        frag.appendChild(note);

        var add = document.createElement("button");
        add.type = "button"; add.style.cssText = TABX.addBtn; add.textContent = "+ Add model";
        add.addEventListener("click", function() { addCard(ui, pool); });
        frag.appendChild(add);
    }

    var root = ui.modelBuilderCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
    applyCompareEnables(ui);
};

module.exports = {

    // Root view update. Bound via `events: update:`; `view_updated` is the jus-3.0 alias -- keep both.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box changed: re-render the reference picker + the model builder (the predictor pool
    // may have changed -> reconcile cards / scaling) + re-apply the survey greying.
    onChange_vars: function(ui) {
        applyWtEnables(ui);
        renderModelTable(ui);
        renderRefPicker(ui);
        renderModelBuilder(ui);
        applyCompareEnables(ui);
    },

    // `compare` changed: force the shared complete-case population for a valid test, re-render the
    // builder (baseline markers show only when compare == "baseline"), re-apply the >=2-models greying.
    onChange_compare: function(ui) {
        forceNaForCompare(ui);
        renderModelBuilder(ui);
        applyCompareEnables(ui);
    },

    // modelBuilderCtrl: build on create. On `updated`, re-render ONLY when the pool / compare changed OR
    // jamovi replaced our $el subtree (marker gone) -- a card / name / marker edit writes models/baseline
    // (NOT in the signature), so it is SKIPPED and the in-place repaint stands.
    modelBuilderCtrl_creating: function(ui) { renderModelBuilder(ui); },
    modelBuilderCtrl_updated:  function(ui) {
        if (!ui.modelBuilderCtrl || !ui.predictors) return;
        var sig  = modelsSig(ui);
        var root = ui.modelBuilderCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-models") === "1");
        if (sig === lastModelsSig && present) return;
        renderModelBuilder(ui);
    },

    // refPickerCtrl: build on create. On `updated`, re-render ONLY when the predictor set changed OR
    // jamovi replaced our $el subtree (marker gone) -- a reference PICK writes refLevels (not in the
    // signature), so it is SKIPPED and the in-place repaint stands.
    refPickerCtrl_creating: function(ui) { renderRefPicker(ui); },
    refPickerCtrl_updated:  function(ui) {
        if (!ui.refPickerCtrl || !ui.predictors) return;
        var sig = refSig(ui);
        var root = ui.refPickerCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-refpick") === "1");
        if (sig === lastRefSig && present) return;
        renderRefPicker(ui);
    },

    // modelTableCtrl: build on create. On `updated`, re-render ONLY when the dependent set / chosen
    // families changed OR jamovi replaced our $el subtree -- a family / level / trials pick writes the
    // hidden depFamily/depModelLevel/depTrials (a family flip IS in the signature, so col-3 repaints),
    // so an in-place edit that keeps the signature is SKIPPED and the DOM edit stands.
    modelTableCtrl_creating: function(ui) { renderModelTable(ui); },
    modelTableCtrl_updated:  function(ui) {
        if (!ui.modelTableCtrl || !ui.dependent) return;
        var sig  = modelTableSig(ui);
        var root = ui.modelTableCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-model") === "1");
        if (sig === lastModelSig && present) return;
        renderModelTable(ui);
    },

    // subtextCtrl: the full-width auto-grow <textarea> for the below-table note (Phase 15c).
    subtextCtrl_creating: function(ui) { renderSubtext(ui); },
    subtextCtrl_updated:  function(ui) { renderSubtext(ui); },

    // extCtrl: the small ".ext" label after the file name; follows the chosen format.
    extCtrl_creating: function(ui) { renderExt(ui); },
    extCtrl_updated:  function(ui) { renderExt(ui); },

    // The chosen format changed: update the file-extension label on the path line.
    export_format_changed: function(ui) {
        renderExt(ui);
    },

    // Reset the export action button shortly after a click, so a second export re-fires the event.
    exportExcel_changed: function(ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function() {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    },

    // Phase h: the "Run comparison" action fires ONE staged compute in the backend; reset it shortly
    // after (like the export button) so a second click re-fires, and so the backend's follow-up run
    // (run_compare = false) re-serves the just-computed table instead of recomputing.
    run_compare_changed: function(ui) {
        if (ui.run_compare && ui.run_compare.value()) {
            setTimeout(function() {
                ui.run_compare.setValue(false);
            }, 2000);
        }
    },

    // Reset the export folder + file name to their defaults, then clear the action so it can re-fire.
    resetPath_changed: function(ui) {
        if (ui.resetPath && ui.resetPath.value()) {
            if (ui.export_dir)      ui.export_dir.setValue("~/Documents");
            if (ui.export_filename) ui.export_filename.setValue("Regression");
            ui.resetPath.setValue(false);
        }
    }

};
