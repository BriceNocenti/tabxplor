// Custom UI events for the jmvtabreg (Regressions) analysis.
// ⚠ Every user-visible string goes through `_()`; the SHARED span's are edited in jmvtab.js.
// See that file's header and dev/tabxplor_2.0.0_jamovi_dev.md § 19.1.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep it
// lean. jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).
// Everything about a PREDICTOR -- its merges, how a number is cut, its baseline, its unit -- is one
// table, built by the SHARED block (copied from jmvtab.js) and described by VAR_TABLE_HOST below.

// --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---
// Generated from R/tab_reg.R (REG_OUTCOME_KINDS), R/reg-estimand.R (REG_FAMILIES,
// REG_ESTIMANDS) and R/var-shape.R (VAR_SHAPES). Re-run dev/generate_jamovi_js.R after
// changing any of them; the suite checks this block (test-jamovi-vocabulary.R).
var TABX_FAMILY_LABEL = { "gaussian": _("gaussian (linear)"), "binomial": _("binomial (logistic)"), "poisson": _("poisson (counts)"), "multinomial": _("multinomial"), "ordinal": _("ordinal") };
var TABX_FAMILY_LABEL_BINARY = { "binomial": _("binomial (logistic)") };
var TABX_OUTCOME_DETECT = { "binary": "binomial", "ordered": "ordinal", "nominal": "multinomial", "numeric": "gaussian" };
var TABX_OUTCOME_OFFERS = { "binary": ["binomial"], "ordered": ["ordinal", "multinomial", "binomial"], "nominal": ["multinomial", "ordinal", "binomial"], "numeric": ["gaussian", "binomial", "poisson"] };
var TABX_LINKS = { "gaussian": ["auto", "difference", "ratio"], "binomial": ["auto", "difference", "ratio", "odds_ratio"], "poisson": ["auto", "ratio"], "multinomial": ["auto", "odds_ratio"], "ordinal": ["auto", "odds_ratio"] };
var TABX_LINK_LABEL = { "auto": _("auto (family based)"), "difference": _("difference"), "ratio": _("ratio"), "odds_ratio": _("odds_ratio") };
var TABX_OUTCOME_LEVEL_ROLE = { "binomial": "modelled", "multinomial": "baseline" };
var TABX_ESTIMANDS = { "gaussian": { "auto": { "auto": ["auto", "difference", "ratio", "raw_coefficient"], "conditional": ["auto", "difference", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio"] }, "difference": { "auto": ["auto", "difference", "ratio", "raw_coefficient"], "conditional": ["auto", "difference", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio"] }, "ratio": { "auto": ["auto", "difference", "ratio", "raw_coefficient"], "conditional": ["auto", "ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio"] } }, "binomial": { "auto": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio", "odds_ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] }, "difference": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "difference", "raw_coefficient"], "marginal": ["auto", "difference", "ratio", "odds_ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] }, "ratio": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio", "odds_ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] }, "odds_ratio": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio", "odds_ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] } }, "poisson": { "auto": { "auto": ["auto", "difference", "ratio", "raw_coefficient"], "conditional": ["auto", "ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio"] }, "ratio": { "auto": ["auto", "difference", "ratio", "raw_coefficient"], "conditional": ["auto", "ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio"] } }, "multinomial": { "auto": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] }, "odds_ratio": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto", "difference", "ratio", "odds_ratio"] } }, "ordinal": { "auto": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto"] }, "odds_ratio": { "auto": ["auto", "difference", "ratio", "odds_ratio", "raw_coefficient"], "conditional": ["auto", "odds_ratio", "raw_coefficient"], "marginal": ["auto", "difference", "ratio"], "at_reference": ["auto"] } } };
var TABX_SHAPES = ["linear", "log", "sqrt", "quadratic", "sd_bands", "median", "terciles", "quartiles", "quintiles", "deciles"];
var TABX_SHAPES_CUT = ["sd_bands", "median", "terciles", "quartiles", "quintiles", "deciles"];
var TABX_SHAPE_LABEL = { "linear": _("linear (numeric)"), "log": _("log (numeric)"), "sqrt": _("sqrt (numeric)"), "quadratic": _("quadratic (num.)"), "sd_bands": _("sd_bands (cut)"), "median": _("median (cut)"), "terciles": _("terciles (cut)"), "quartiles": _("quartiles (cut)"), "quintiles": _("quintiles (cut)"), "deciles": _("deciles (cut)") };
// --- END GENERATED ---

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

// (Phase 19k: applyWtEnables() was DELETED. It greyed `ids` / `strata` / `fpc` / `nest`, the survey
//  -design controls REMOVED in z14-i -- four options that exist in neither .a.yaml nor .h.R, so the
//  function had been a no-op running on every update. A prebuilt survey::svydesign is the R-side
//  route for anything richer than a weight; the module offers `wt` only.)

// WHY the path boxes ignored their stretch: jamovi compiles every grid cell to
// `minmax(max-content, <stretch>fr)`, so a control claims its min-content width BEFORE the stretch
// factors divide the row. A `width: largest` TextBox has a ~200px min (class silky-option-largest-text),
// so both boxes bottom out at ~200px. Collapse that floor with a persistent <style> (inline styles are
// dropped on jamovi re-renders). Folder keeps a 260px minimum so it stays the wider box (tune to taste);
// the file name (width: large) collapses fully. Only the two export boxes use these width classes.
// Phase h: also spaces the options collapse boxes apart (breathing room below each pane). The collapse-
// box. Phase 18r: the empty line at the BOTTOM of each collapse box (blank only while EXPANDED, so a
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

// Phase 18r: the "Run comparison" action matches the Export button -- jamovi's DEFAULT ActionButton
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
    applyVarEnables(ui);
    renderSubtext(ui);
    renderExt(ui);
    styleResetBtn(ui);
    styleExportSep(ui);                      // Phase o: thin rule above the (out-of-hierarchy) Export block
    styleRunCompareBtn(ui);                  // Phase 18r: default (Export-look) button + blank line
    bottomAlignInRow(ui, "export_format");   // Format combo -> bottom of row 1 (aligns with Export button)
    bottomAlignInRow(ui, "extCtrl");         // ".ext" text -> bottom of the path row
    renderModelTable(ui);
    renderVarTable(ui);
    renderModelBuilder(ui);        // ...and the interactions, which live inside it since 22g-viii
};

// Phase 15d: a valid model-comparison test needs every model fit on the SAME cases -- so only
// `drop_by_model` (each model on its own complete cases) breaks it, and having a comparison pushes
// back to the default `drop_by_outcome`, which already fits every model of ONE outcome on one
// population (a comparison has a single outcome). Guarded setValue -> idempotent, no update loop.
// ⚠ Phase 22g-iii re-keyed it on the CARD COUNT: the `stats_compare` picker it used to watch is
// gone, and since 22g-ii two or more subsets are compared automatically -- so without this the
// comparison would silently fall back to a bare AIC difference under `drop_by_model`.
var forceNaForCompare = function(ui) {
    if (!ui.models || !ui.na) return;
    var n = utils.clone(ui.models.value(), []).length;
    if (n >= 2 && ui.na.value() === "drop_by_model") ui.na.setValue("drop_by_outcome");
};

// --- BEGIN SHARED (dev/generate_jamovi_js.R: copied from jamovi/js/jmvtab.js) -- do not edit ---
// Phase 22g-iv: THE PER-VARIABLE TABLE -- the ONE widget both analyses show, and the one place a
// user answers every question that is about a VARIABLE rather than about the table: how its levels
// are ordered, which of them are merged, how a number is cut into groups, what it is compared to,
// and (regressions) in what unit its effect is read. One row per variable, one column per question,
// the merge list opening inline underneath the row it belongs to.
//
// It is SELF-CONTAINED by construction -- its own styles, its own state, its own column fetch, its
// own generic option helpers, and no reference to either file's TABX object -- because the
// generator copies it verbatim into jmvtabreg.js and `check` mode fails on drift.
//
// The per-panel half is ONE declared object, VAR_TABLE_HOST, written outside these markers: it says
// which variables the table lists, which columns it has, what a `shape` drop-down offers, and what
// the reference / scaling cell of one variable is. Everything else is here, written once.
//
// ⚠ THE SIGNATURE RULE. host.sig() must name ONLY what the table does not itself write (the
// variable boxes, and in jmvtab the options that decide which axis is compared). Every option the
// widget writes -- levels_order, levels_collapse, shape, ref_levels, ref2, multiplier -- is OUT of
// it, and is repainted IN PLACE (tabxvRefreshVar) instead. Putting one back in would rebuild the
// whole table on every merge tick and every reorder move, which is the "2nd click does nothing,
// then all changes appear later" bug: the rebuild clobbers the in-place edit that caused it.
var TABXV = {
    wrap:   "padding:2px 6px 6px 6px;width:100%;box-sizing:border-box;",
    // ⚠ ONE grid PER GROUP of variables, and it holds that group's head row AND its data rows, so a
    // header can never drift from the column it names. The group's own name IS the first column's
    // head ("Row variables", "Predictors"), which is why there is a single header row.
    // grid-template-columns comes from host.cols: the name column is the only elastic one and every
    // other is FIXED IN PIXELS -- an `auto` or `1fr` control column resizes the widget the moment a
    // control appears or a longer value is typed. ⚠ Its minmax FLOOR is load-bearing too: with
    // minmax(0,1fr) and 510px of fixed columns the name column collapsed to nothing in a narrow
    // options pane, and the variable names simply were not there.
    grid:   "display:grid;align-items:stretch;width:100%;box-sizing:border-box;margin:2px 0 10px 0;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#E4E4E4;color:#000;overflow:hidden;",
    head:   "padding:4px 8px;font-weight:600;color:#000;background:#CCCCCC;border-bottom:1px solid rgba(0,0,0,0.18);white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
    cell:   "padding:3px 6px;display:flex;align-items:center;min-width:0;border-top:1px solid rgba(0,0,0,0.10);",
    name:   "padding:3px 6px;display:flex;align-items:center;gap:2px;min-width:0;border-top:1px solid rgba(0,0,0,0.10);",
    varNm:  "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    caret:  "display:inline-block;width:1.1em;opacity:0.7;",
    lvlBtn: "cursor:pointer;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
    dotted: "border-bottom:1px dotted rgba(0,0,0,0.45);",
    note:   "opacity:0.55;font-style:italic;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
    sel:    "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    unit:   "display:flex;align-items:center;gap:3px;min-width:0;white-space:nowrap;",
    inp:    "width:52px;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;",
    // ⚠ the LEFT padding is not decoration: it is what says the open list belongs to the row
    // above it rather than to the group. Kept modest -- the options pane is the narrowest thing
    // jamovi shows, and a real 1cm would eat the merged-name box.
    exp:    "padding:0 6px 4px 26px;background:#E4E4E4;border-top:1px solid rgba(0,0,0,0.10);",
    // the "click to ..." half of the level opener: an instruction, not a fact, so it is set back
    // to the chrome's own aside grey (tx_chrome_hex()$grey2 in the light theme).
    lvlHow: "font-style:italic;color:#444444;",
    hint:   "padding:8px;opacity:0.65;font-style:italic;"
};

// State persisting across rebuilds. levelsCache makes the render SYNCHRONOUS after the first fetch
// -- a deferred async swap was racing the user's in-place edits.
var levelsCache = {};     // var -> [labels] natural order | null (numeric / no levels) | FETCHING
var mtypeCache  = {};     // var -> jamovi measureType ("nominal" | "ordinal" | "continuous" | "id")
var FETCHING    = {};
var tabxvOpen   = {};     // var -> is its level list open
var tabxvCells  = {};     // var -> {levels, ref, act, exp, caret, setOpen} for THIS render
var tabxvLastSig = null;  // the last host.sig() rendered

// --- cleannames: the SAME rule R applies, so the panel shows the table's own words --------------
// tab(cleannames = TRUE) is the jamovi default, so `2-Catholic` reaches the user as `Catholic` --
// and a widget showing the raw name teaches a level that is not in the table. This is
// cleannames_condition() (R/utils.R) transcribed: strip a leading `token-` prefix (only where the
// token does not end in a lowercase letter, so `non-white` survives) and any ` (...)` group.
// ⚠ EVERY stored value stays RAW -- `data-lab`, `<option value>`, levels_collapse, levels_order,
// ref_levels. Only what a human reads is cleaned.
// ⚠ Built with `new RegExp` inside a try: the lookbehind would be a PARSE error on an old engine,
// which would take the whole file down rather than one label's prefix.
var TABXV_CLEAN = (function () {
    try { return new RegExp("^[^- ]+-(?!\\p{Ll})|^[^- ]+(?<!\\p{Ll})-| *\\(.+\\)", "gu"); }
    catch (e) { try { return new RegExp("^[^- ]+-(?![a-z])| *\\(.+\\)", "g"); } catch (e2) { return null; } }
})();
var tabxvClean = function (ui, lab) {
    if (!TABXV_CLEAN || !ui.cleannames || !ui.cleannames.value()) return String(lab);
    return String(lab).replace(TABXV_CLEAN, "");
};
// The default name of a merged run: the FIRST level whole, the followers cleaned, so no prefix ever
// lands mid-name (`1-Protestant, Catholic`). The same rule R applies in new_lvl_collapse().
var tabxvRunLabel = function (levels) {
    if (!TABXV_CLEAN) return levels.join(", ");
    return [levels[0]].concat(levels.slice(1).map(function (l) {
        return String(l).replace(TABXV_CLEAN, "");
    })).join(", ");
};

// --- the option surface: three verbs over a {var, <key>} Array, and one select ----------------
// ⚠ Every helper GUARDS on its option existing. A hidden, control-less option only resolves through
// `ui.<name>` once the generated .h.R declares it, and that file LAGS a .a.yaml edit until the next
// jmvtools::prepare() -- an unguarded read there throws and takes the whole `update` handler with
// it, so the panel goes inert rather than degrading.
// These take a SCALAR-valued key; an array-valued one (levels_order, levels_collapse) has its own
// reader below, because a stored level list is not a String.
var arrGet = function (ui, opt, v, key) {
    if (!ui[opt]) return "";
    var arr = utils.clone(ui[opt].value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i][key] == null ? "" : String(arr[i][key]));
    return "";
};

// Set/replace a per-variable {var, <key>: val} entry; a BLANK val removes it -> the backend default.
// Guarded (JSON compare) so an unchanged pick never re-fires `update`.
var arrWrite = function (ui, opt, v, key, val) {
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

// Drop entries whose variable is no longer selected (guarded setValue -> no loop).
var reconcileArr = function (ui, opt, deps) {
    if (!ui[opt]) return;
    var cur = utils.clone(ui[opt].value(), []);
    var kept = cur.filter(function (e) { return deps.indexOf(e.var) >= 0; });
    if (kept.length !== cur.length) ui[opt].setValue(kept);
};

// `labelOf` may be a map or a function; the option VALUE is always the raw one.
var makeSelect = function (style, options, labelOf, selected, onPick) {
    var sel = document.createElement("select"); sel.style.cssText = style;
    options.forEach(function (o) {
        var opt = document.createElement("option");
        opt.value = o;
        opt.textContent = (typeof labelOf === "function") ? labelOf(o)
                        : ((labelOf && labelOf[o]) ? labelOf[o] : o);
        if (o === selected) opt.selected = true;
        sel.appendChild(opt);
    });
    sel.addEventListener("change", function () { onPick(sel.value); });
    return sel;
};

// --- the level ORDER: the one array-valued option, and the one a host may not have -----------
// ⚠ It is reached through `host.orderOpt`, never by name: the two panels declare it under the same
// name but drive DIFFERENT things with it (tab(levels_order =) against a jmvtabreg-only relevel of
// the predictor columns before the fit), and a host may have none at all.
// The order to display for `v`: the stored entry (kept for still-present levels, new levels
// appended) if any, else the column's natural level order.
var tabxvStoredOrder = function (ui, host, v, natural) {
    if (!host.orderOpt || !ui[host.orderOpt]) return natural;
    var arr = utils.clone(ui[host.orderOpt].value(), []);
    for (var i = 0; i < arr.length; i++) {
        if (arr[i].var === v && arr[i].levels && arr[i].levels.length) {
            var out = [];
            arr[i].levels.forEach(function (l) {
                if (natural.indexOf(l) >= 0 && out.indexOf(l) < 0) out.push(l);
            });
            natural.forEach(function (l) { if (out.indexOf(l) < 0) out.push(l); });
            return out;
        }
    }
    return natural;
};

// Store a COPY of `lv` -- never the caller's live working array, else later in-place swaps would
// alias the option value and setValue() could miss the change.
var tabxvWriteOrder = function (ui, host, v, lv) {
    if (!host.orderOpt || !ui[host.orderOpt]) return;
    var copy = lv.slice();
    var arr = utils.clone(ui[host.orderOpt].value(), []), found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, levels: copy }; found = true; break; }
    if (!found) arr.push({ var: v, levels: copy });
    ui[host.orderOpt].setValue(arr);
};

// --- the column fetch, written once ----------------------------------------------------------
// `measureType` is kept beside the labels because an ORDERED factor may be merged but not reordered,
// and a CONTINUOUS one is offered a `shape` instead of a level list -- both are facts about the
// column, not about the caller.
var fetchLevels = function (ui, host, v) {
    if (!v || (v in levelsCache) || !ui[host.ctrl]) return;
    levelsCache[v] = FETCHING;                       // guard against duplicate in-flight fetches
    ui[host.ctrl].requestData("column", { columnName: v, properties: ["measureType", "levels"] })
        .then(function (col) {
            mtypeCache[v]  = col ? col.measureType : "continuous";
            levelsCache[v] = (!col || col.measureType === "continuous")
                ? null : col.levels.map(function (l) { return l.label; });
            tabxvRender(ui, host);
        })
        .catch(function () {
            mtypeCache[v] = "continuous"; levelsCache[v] = null; tabxvRender(ui, host);
        });
};
// The cached labels, or `undefined` while the fetch is in flight (which is what draws a placeholder).
var cachedLevels = function (v) {
    var c = (v in levelsCache) ? levelsCache[v] : undefined;
    return (c === FETCHING) ? undefined : c;
};

// --- what a variable IS, computed once so the two hosts cannot disagree ----------------------
// ⚠ `isCut` is TRUE for a number on an axis that cannot hold one (a crosstab's row / table axis):
// there `shape = "auto"` still cuts -- it is the ABSENCE of a stored value, not a refusal to cut --
// so the variable does have groups, and the positional references name them.
var tabxvKind = function (ui, host, g, v) {
    var cached  = cachedLevels(v);
    var offered = host.shapes(g);
    var shape   = arrGet(ui, "shape", v, "shape");
    if (!shape || offered.indexOf(shape) < 0) shape = offered[0];
    var isNumber = (cached === null);
    return {
        group: g, cached: cached, mtype: mtypeCache[v], offered: offered,
        defShape: offered[0], shape: shape,
        loading:  (cached === undefined),
        isNumber: isNumber,
        isCut:    isNumber && (!g.numericMayKeep || host.isCut(shape))
    };
};

var tabxvGroupOf = function (ui, host, v) {
    var gs = host.groups(ui);
    for (var i = 0; i < gs.length; i++) if (gs[i].vars.indexOf(v) >= 0) return gs[i];
    return null;
};

// The levels the TABLE will show, in display order, for one variable.
var tabxvLevels = function (ui, host, v, natural) {
    return tabxmDisplayOrder(tabxvStoredOrder(ui, host, v, natural || []), tabxmGroups(ui, v));
};

// --- one row --------------------------------------------------------------------------------
// Column 2 says what the variable's levels ARE and is the opener for the list: "8 levels: click to
// relevel" collapsed, "8 levels" open. ⚠ the count is the ORIGINAL one -- a merge is a statement
// about those levels, not a new set of them, and a count that fell to 2 told the user nothing.
var tabxvFillLevels = function (ui, host, v, kind) {
    var c = tabxvCells[v]; if (!c || !c.levels) return;
    c.levels.innerHTML = "";
    if (kind.loading) {
        var w = document.createElement("span"); w.style.cssText = TABXV.note;
        w.textContent = "…"; c.levels.appendChild(w); return;
    }
    if (kind.isNumber) {                       // a NUMBER: the one question it raises is how it is cut
        if (!ui.shape) return;                 // the generated .h.R lags -- show no dead control
        var sel = makeSelect(TABXV.sel, kind.offered, TABX_SHAPE_LABEL, kind.shape, function (val) {
            arrWrite(ui, "shape", v, "shape", (val === kind.defShape) ? "" : val);
            if (host.varSync) host.varSync(ui, v, tabxvKind(ui, host, kind.group, v));
            tabxvRefreshVar(ui, host, v);
        });
        sel.title = _("how this number becomes groups (shape =)");
        c.levels.appendChild(sel); return;
    }
    var natural = kind.cached || [];
    if (natural.length === 0) return;
    // the COUNT is the original one -- a merge is a statement about those levels, not a new set of
    // them -- and the instruction beside it says which way the click goes.
    var b = document.createElement("span"); b.style.cssText = TABXV.lvlBtn + TABXV.dotted;
    var n = document.createElement("span");
    n.textContent = _("{n} levels").replace("{n}", String(natural.length));
    var h = document.createElement("span"); h.style.cssText = TABXV.lvlHow;
    h.textContent = " \u2014 " + (tabxvOpen[v] ? host.closeTip : host.mergeTip);
    b.appendChild(n); b.appendChild(h);
    b.addEventListener("click", function () { c.setOpen(!tabxvOpen[v]); });
    c.levels.appendChild(b);
};

// The remaining cells: the reference, and (where the panel has one) the scaling of a number.
var tabxvFillRest = function (ui, host, v, kind) {
    var c = tabxvCells[v]; if (!c) return;
    if (c.ref) { c.ref.innerHTML = ""; if (host.refCell) host.refCell(ui, c.ref, v, kind); }
    if (!c.act) return;
    c.act.innerHTML = "";
    if (kind.isNumber && host.unitCell) host.unitCell(ui, c.act, v, kind);
};

// Rebuild `v`'s OPEN level list from the stored order. ⚠ Only for a change made OUTSIDE the list
// (jmvtabreg's `ref =` cell reorders): calling it from the list's own onCommit would detach the
// grid that handler is about to repaint.
var tabxvRebuildList = function (ui, host, v) {
    var c = tabxvCells[v];
    if (!c || !c.exp || !tabxvOpen[v] || !c.setOpen) return;
    c.exp.innerHTML = "";
    c.setOpen(true);
};

// Repaint everything about ONE variable that a merge, a reorder or a `shape` pick can change --
// without rebuilding the table, which is what keeps the in-place edit that triggered it alive.
var tabxvRefreshVar = function (ui, host, v) {
    var g = tabxvGroupOf(ui, host, v); if (!g || !tabxvCells[v]) return;
    var kind = tabxvKind(ui, host, g, v);
    tabxvFillLevels(ui, host, v, kind);
    tabxvFillRest(ui, host, v, kind);
};

var tabxvRow = function (ui, host, grid, g, v) {
    var kind = tabxvKind(ui, host, g, v);
    fetchLevels(ui, host, v);
    var c = tabxvCells[v] = {};
    host.cols.forEach(function (col) {
        var d = document.createElement("div");
        d.style.cssText = (col.key === "name") ? TABXV.name : TABXV.cell;
        grid.appendChild(d);
        c[col.key] = d;
    });
    // the level list, in a full-width cell of the SAME grid -- so opening one never disturbs a column
    var exp = document.createElement("div");
    exp.style.cssText = TABXV.exp; exp.style.gridColumn = "1 / -1"; exp.style.display = "none";
    grid.appendChild(exp);
    c.exp = exp;

    var canExpand = !kind.loading && !kind.isNumber && kind.cached && kind.cached.length > 0;
    var caret = document.createElement("span"); caret.style.cssText = TABXV.caret;
    caret.textContent = canExpand ? "▸" : " ";
    var nm = document.createElement("b"); nm.style.cssText = TABXV.varNm; nm.textContent = v;
    if (c.name) { c.name.appendChild(caret); c.name.appendChild(nm); }
    c.caret = caret;

    c.setOpen = function (open) {
        if (!canExpand) return;
        tabxvOpen[v] = !!open;
        caret.textContent = open ? "▾" : "▸";
        exp.style.display = open ? "block" : "none";
        if (open && !exp.firstChild) {
            // An ORDERED factor keeps its merge tick-boxes and loses its arrows: moving one level of
            // an ordered scale is meaningless, merging two contiguous ones is not.
            var order = tabxvStoredOrder(ui, host, v, kind.cached);
            exp.appendChild(tabxmBuildList(
                ui, v, order,
                host.orderOpt ? function (o) { tabxvWriteOrder(ui, host, v, o); } : null,
                mtypeCache[v] !== "ordinal",
                function () { if (host.varSync) host.varSync(ui, v, kind);
                              tabxvRefreshVar(ui, host, v); },
                host.boldFirst));
        }
        tabxvFillLevels(ui, host, v, kind);          // the opener's wording follows the state
    };
    if (canExpand && c.name) {
        c.name.style.cursor = "pointer";
        c.name.addEventListener("click", function () { c.setOpen(!tabxvOpen[v]); });
    }
    tabxvFillLevels(ui, host, v, kind);
    tabxvFillRest(ui, host, v, kind);
    if (canExpand && tabxvOpen[v]) c.setOpen(true);
};

// --- the whole table, rendered SYNCHRONOUSLY into $el ----------------------------------------
// ONE grid per group of variables, each with its own single header row whose first column is named
// by the group ("Row variables", "Predictors"), and a little air between them. The
// `data-tabx-vartable` marker lets the `updated` handler tell a jamovi $el re-render (marker gone
// -> rebuild) from a plain option write (marker present + same signature -> skip).
var tabxvRender = function (ui, host) {
    var ctrl = ui[host.ctrl];
    if (!ctrl || !ctrl.$el || !ctrl.$el[0]) return;
    var groups = host.groups(ui), all = [];
    groups.forEach(function (g) {
        g.vars.forEach(function (v) { if (all.indexOf(v) < 0) all.push(v); });
    });
    tabxvLastSig = host.sig(ui);
    tabxvCells = {};

    if (host.orderOpt) reconcileArr(ui, host.orderOpt, all);
    tabxmReconcile(ui, all);
    reconcileArr(ui, "shape", all);
    if (host.reconcile) host.reconcile(ui, all);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-vartable", "1");
    frag.style.cssText = TABXV.wrap;
    if (all.length === 0) {
        var h = document.createElement("div"); h.style.cssText = TABXV.hint;
        h.textContent = host.emptyHint;
        frag.appendChild(h);
    } else {
        var tmpl = host.cols.map(function (col) { return col.width; }).join(" ");
        groups.forEach(function (g) {
            if (g.vars.length === 0) return;
            var grid = document.createElement("div");
            grid.style.cssText = TABXV.grid;
            grid.style.gridTemplateColumns = tmpl;
            host.cols.forEach(function (col, k) {
                var hd = document.createElement("div"); hd.style.cssText = TABXV.head;
                // A group may RE-HEAD a column, because the same cell can hold a different argument
                // on a different group (jmvtab's `ref` column holds `ref2` off the percentage axis).
                // "" IS a head -- "this column holds nothing here" -- while undefined keeps col.head.
                var txt = (g.heads && g.heads[col.key] !== undefined) ? g.heads[col.key] : col.head;
                hd.innerHTML = (k === 0) ? (g.label || txt) : txt;             // our own strings only
                if (col.tip) hd.title = col.tip;
                grid.appendChild(hd);
            });
            g.vars.forEach(function (v) { tabxvRow(ui, host, grid, g, v); });
            frag.appendChild(grid);
        });
    }
    var root = ctrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

// The `updated` guard both files share: rebuild only when the signature moved or jamovi replaced
// our subtree. Everything the widget writes is outside the signature (see the header) and repaints
// in place, so the user's own click never rebuilds the table under the pointer.
var tabxvUpdated = function (ui, host) {
    var ctrl = ui[host.ctrl];
    if (!ctrl || !ctrl.$el || !ctrl.$el[0]) return;
    var root = ctrl.$el[0];
    var present = !!(root.firstChild && root.firstChild.getAttribute &&
                     root.firstChild.getAttribute("data-tabx-vartable") === "1");
    if (present && host.sig(ui) === tabxvLastSig) return;
    tabxvRender(ui, host);
};

// --- the level list, with the merge tick-boxes -----------------------------------------------
// THE MODEL. A tick on level i means "merge this level into the run above"; the first level of the
// displayed order never has one, and chained ticks make a run. A tick belongs to the LEVEL, not to
// the position, so moving a level up or down simply re-forms the runs and the list stays WYSIWYG.
// The stored option is order-INDEPENDENT groups ({var, label, levels}), one entry per merged run, so
// R can apply it with forcats::fct_collapse() and the order stays `levels_order`'s business.
// The merged-name box writes on `change`/`blur`, NEVER on `input`: jamovi recomputes the analysis on
// every option write, and a per-keystroke write would recompute per character. Left empty it shows
// the joined level labels as a PLACEHOLDER -- the default itself lives in R (new_lvl_collapse), once.
var TABXM_SEL = "#b5caef";       // jamovi's list-selection blue (.selected in analysisui.css)
var TABXM = {
    body:  "padding:2px 0 6px 0;width:100%;box-sizing:border-box;",
    // 3 columns: level | merge tick | merged name. The name cell spans its run with grid-row/span,
    // which is why this is a grid and not the <ul> it replaced.
    // ⚠ THE TWO RIGHT COLUMNS ARE FIXED IN PIXELS, AND THE BOX IS width:100%. Everything else made
    // the widget resize under the pointer: `auto` sized column 2 to the tick-box (so it changed the
    // moment one appeared), `minmax(96px,1fr)` grew column 3 with whatever was typed in it, and the
    // overflow scrollbar took width away on expand -- `scrollbar-gutter:stable` reserves it always.
    grid:  "display:grid;grid-template-columns:minmax(0,1fr) 72px 200px;align-items:stretch;margin:4px 0;border:1px solid rgba(0,0,0,0.25);border-radius:3px;background:#F0F0F0;color:#000;max-height:220px;overflow-y:auto;scrollbar-gutter:stable;outline:none;width:100%;box-sizing:border-box;",
    // NOT a header row -- there is none. This is the word "merge" written into the ONE tick cell
    // the grid leaves empty by construction (the first level has nothing above it to merge into),
    // so the column names itself where it starts instead of costing a row.
    head:  "padding:2px 6px;font-size:0.9em;font-style:italic;color:#444444;display:flex;align-items:center;justify-content:center;white-space:nowrap;",
    lab:   "padding:2px 8px;cursor:pointer;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:flex;align-items:center;",
    tick:  "padding:2px 6px;display:flex;align-items:center;justify-content:center;cursor:pointer;",
    cell:  "padding:2px 4px;display:flex;align-items:center;",
    input: "width:100%;min-width:0;box-sizing:border-box;padding:1px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;",
    bar:   "display:flex;gap:6px;",
    btn:   "width:30px;height:22px;line-height:1;padding:0;cursor:pointer;",
    btnOff:"width:30px;height:22px;line-height:1;padding:0;opacity:0.4;cursor:default;"
};
var tabxmSel = {};      // var -> selected level label (persists across rebuilds)

// --- the option: read, write, reconcile ---------------------------------------------------
// The groups stored for `v`, as [{label, levels}] (the option is flat: `var` repeats per group).
var tabxmGroups = function (ui, v) {
    if (!ui.levels_collapse) return [];
    var arr = utils.clone(ui.levels_collapse.value(), []), out = [];
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v && arr[i].levels && arr[i].levels.length > 1)
            out.push({ label: arr[i].label || "", levels: arr[i].levels.slice() });
    return out;
};

// Replace ALL of `v`'s groups (other variables' entries untouched). Store a COPY of every level
// array -- setValue may keep it by reference, and a later in-place edit would then alias the option.
var tabxmWrite = function (ui, v, groups) {
    if (!ui.levels_collapse) return;
    var arr = utils.clone(ui.levels_collapse.value(), []), kept = [];
    for (var i = 0; i < arr.length; i++) if (arr[i].var !== v) kept.push(arr[i]);
    groups.forEach(function (g) {
        if (g.levels.length > 1)
            kept.push({ var: v, label: g.label || "", levels: g.levels.slice() });
    });
    ui.levels_collapse.setValue(kept);
};

// Drop entries whose variable is no longer selected (guarded setValue -> no loop).
var tabxmReconcile = function (ui, selected) {
    if (!ui.levels_collapse) return;
    var cur = utils.clone(ui.levels_collapse.value(), []), kept = [];
    for (var i = 0; i < cur.length; i++)
        if (selected.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.levels_collapse.setValue(kept);
};

// --- groups <-> ticks, and the post-merge display order ------------------------------------
// A tick is per LEVEL: true when this level is in the same group as the one before it in `order`.
var tabxmTicks = function (order, groups) {
    var runOf = {};
    groups.forEach(function (g, k) { g.levels.forEach(function (l) { runOf[l] = k; }); });
    return order.map(function (l, i) {
        return i > 0 && runOf[l] !== undefined && runOf[l] === runOf[order[i - 1]];
    });
};

// Rebuild the groups from the ticks, carrying each run's typed label over: an exact level-set match
// first, else the label of the group that held the run's first level (so extending a run keeps it).
var tabxmFromTicks = function (order, ticks, old) {
    var runs = [], cur = null;
    order.forEach(function (l, i) {
        if (i > 0 && ticks[i]) cur.push(l); else { cur = [l]; runs.push(cur); }
    });
    var labelFor = function (levels) {
        var key = levels.slice().sort().join(""), byFirst = "";
        for (var k = 0; k < old.length; k++) {
            if (old[k].levels.slice().sort().join("") === key) return old[k].label || "";
            if (!byFirst && old[k].levels.indexOf(levels[0]) >= 0) byFirst = old[k].label || "";
        }
        return byFirst;
    };
    return runs.filter(function (r) { return r.length > 1; })
               .map(function (r) { return { label: labelFor(r), levels: r }; });
};

// The levels the TABLE will show, in display order: each run replaced by its merged label (its
// joined levels when the box is empty -- the same default R applies). Used by the reference cells,
// whose choices must be levels that still exist after the merge.
var tabxmDisplayOrder = function (order, groups) {
    var labOf = {};
    groups.forEach(function (g) {
        var lab = g.label || g.levels.join(", ");
        g.levels.forEach(function (l) { labOf[l] = lab; });
    });
    var out = [];
    order.forEach(function (l) {
        var lab = (labOf[l] !== undefined) ? labOf[l] : l;
        if (out.indexOf(lab) < 0) out.push(lab);
    });
    return out;
};

// --- the widget ----------------------------------------------------------------------------
// `onOrder(newOrder)` is supplied by a host that also offers reordering (jmvtab) and is null
// otherwise (jmvtabreg, whose producer has no `levels_order` argument to write to); the up/down bar
// and the arrow keys appear only when it is given.
// `canOrder = false` keeps the bar but GREYS it: an ORDERED factor already has the order its levels
// mean, so moving one is meaningless -- while merging two contiguous ordinal levels is not, and its
// tick-boxes stay live.
// `onCommit()` fires after every write (a tick or a move): the levels this variable HAS just
// changed, so its reference cell -- which offers exactly those levels -- has to follow.
// `boldFirst` marks the first level: where the producer's baseline IS the first level (tab_reg),
// the list and the reference cell are two views of one fact, and the bold says so.
var tabxmBuildList = function (ui, v, initialOrder, onOrder, canOrder, onCommit, boldFirst) {
    if (canOrder === undefined) canOrder = true;
    var wrap = document.createElement("div");
    wrap.style.cssText = TABXM.body;
    var order  = initialOrder.slice();
    var groups = tabxmGroups(ui, v);
    var ticks  = tabxmTicks(order, groups);
    // ⚠ `R/jmvtab.h.R` is GENERATED, so between a `.a.yaml` edit and the maintainer's next
    // jmvtools::prepare() the option simply does not exist. Show the level list WITHOUT the merge
    // columns rather than tick-boxes that write nowhere -- the .js half of the `%||%` discipline.
    var canMerge = !!ui.levels_collapse;

    var grid = document.createElement("div");
    grid.style.cssText = TABXM.grid; grid.tabIndex = 0;
    if (!canMerge) grid.style.gridTemplateColumns = "1fr";

    var selected = function () {
        var s = tabxmSel[v];
        return (s && order.indexOf(s) >= 0) ? s : order[0];
    };
    var paint = function () {
        var sel = selected();
        Array.prototype.forEach.call(grid.querySelectorAll("[data-lab]"), function (el) {
            el.style.background = (el.getAttribute("data-lab") === sel) ? TABXM_SEL : "";
        });
    };
    var commit = function () {
        ticks[0] = false;                                  // the first level can never merge upwards
        groups = tabxmFromTicks(order, ticks, groups);
        tabxmWrite(ui, v, groups);
        if (onCommit) onCommit();
    };
    var cell = function (style, col, row, span) {
        var d = document.createElement("div");
        d.style.cssText = style;
        d.style.gridColumn = String(col);
        d.style.gridRow = span ? (String(row) + " / span " + String(span)) : String(row);
        return d;
    };
    // ⚠ NO HEADER ROW: the three heads cost a row of a box that is already tall, and each column
    // says what it is anyway -- the levels are levels, the name box shows its default as a
    // placeholder, and "merge" is written into the first tick cell, which is empty by construction.
    var renderRows = function () {
        grid.innerHTML = "";
        order.forEach(function (lab, i) {
            var row = i + 1;
            var l = cell(TABXM.lab, 1, row);
            l.setAttribute("data-lab", lab);              // the RAW name: it is what is stored
            l.textContent = tabxvClean(ui, lab);
            if (boldFirst && i === 0) l.style.fontWeight = "700";
            l.addEventListener("click", function () { tabxmSel[v] = lab; paint(); grid.focus(); });
            grid.appendChild(l);
            if (!canMerge) return;

            var t = cell(i > 0 ? TABXM.tick : TABXM.head, 2, row);
            if (i === 0) t.textContent = _("merge");   // the column names itself in its empty cell
            if (i > 0) {                      // the first level has nothing above to merge into
                var cb = document.createElement("input");
                cb.type = "checkbox"; cb.checked = !!ticks[i];
                cb.title = _("merge into the level above");
                cb.addEventListener("change", function () {
                    ticks[i] = cb.checked; commit(); renderRows();
                });
                t.appendChild(cb);
            }
            grid.appendChild(t);
        });
        if (!canMerge) { paint(); return; }
        // the merged-name boxes: one per run, spanning it with grid-row/span. Runs of one still get
        // an empty cell: the grid needs every row of column 3 occupied, or the spans below it slide.
        var cur = tabxmFromTicks(order, ticks, groups);
        var i = 0;
        while (i < order.length) {
            var j = i + 1;
            while (j < order.length && ticks[j]) j++;
            var len = j - i, c = cell(TABXM.cell, 3, i + 1, len);
            if (len > 1) {
                var levels = order.slice(i, j);
                var box = document.createElement("input");
                box.type = "text"; box.style.cssText = TABXM.input;
                box.value = (cur.filter(function (g) { return g.levels[0] === levels[0]; })[0]
                             || {}).label || "";
                box.placeholder = tabxvClean(ui, tabxvRunLabel(levels));
                // ⚠ `box` and `levels` are BOTH passed into the closure. They are `var`s inside a
                // while loop, i.e. one function-scoped binding shared by every handler -- so a
                // handler reading them directly would edit the LAST run whichever box was typed in.
                // `change` (commit on blur / Enter), NEVER `input`: see the header.
                box.addEventListener("change", function (lv, b) {
                    return function () {
                        var val = b.value.trim();
                        groups = tabxmFromTicks(order, ticks, groups);
                        groups.forEach(function (g) { if (g.levels[0] === lv[0]) g.label = val; });
                        tabxmWrite(ui, v, groups);
                        if (onCommit) onCommit();
                    };
                }(levels, box));
                c.appendChild(box);
            }
            grid.appendChild(c);
            i = j;
        }
        paint();
    };
    var move = function (dir) {
        var sel = selected();
        var i = order.indexOf(sel), j = i + dir;
        if (j < 0 || j >= order.length) return;
        order[i] = order[j]; order[j] = sel;    // swap: the selected level moves to j
        tabxmSel[v] = sel;                      // selection follows it, so repeated moves walk it
        // A tick belongs to the LEVEL, so the runs simply re-form around the new order and a merge
        // follows its levels. The one visible consequence, and it is the honest one: moving a level
        // INTO a run splits it, and a run that is no longer contiguous stops being a merge -- the
        // ticks disappear where the user can see them, rather than a non-contiguous group being
        // kept behind a display that shows it as separate levels.
        ticks = tabxmTicks(order, groups);
        // ⚠ THE ORDER IS WRITTEN FIRST. `commit()` fires `onCommit`, and a host that derives
        // something FROM the order (jmvtabreg: its reference IS the first level) reads the option
        // back -- so writing it after left every such read one move behind, which is exactly what
        // the bold first level and the `ref =` cell disagreeing looked like.
        onOrder(order);
        commit();
        renderRows();
    };
    if (onOrder && canOrder) {
        // ⚠ ignore the arrow keys while a tick-box or the name box has focus, or typing a merged
        // label would reorder the levels underneath it.
        grid.addEventListener("keydown", function (e) {
            if (e.target && e.target.tagName === "INPUT") return;
            if (e.key === "ArrowUp")        { e.preventDefault(); move(-1); }
            else if (e.key === "ArrowDown") { e.preventDefault(); move(1); }
        });
    }
    renderRows();
    wrap.appendChild(grid);

    if (onOrder) {
        var bar = document.createElement("div");
        bar.style.cssText = TABXM.bar;
        var mk = function (sym, dir) {
            var b = document.createElement("button");
            b.type = "button"; b.textContent = sym;
            b.style.cssText = canOrder ? TABXM.btn : TABXM.btnOff;
            if (!canOrder) {
                b.disabled = true;
                b.title = _("an ordered variable already has the order its levels mean");
                return b;
            }
            b.addEventListener("click", function (e) { e.preventDefault(); grid.focus(); move(dir); });
            return b;
        };
        bar.appendChild(mk("▲", -1));
        bar.appendChild(mk("▼",  1));
        wrap.appendChild(bar);
    }
    return wrap;
};
// --- END SHARED ---

// ---- The regression host: what the shared table lists, and what its cells are ----------------
// One group (the predictors), no up/down bar -- tab_reg() has no `levels_order` argument, so a move
// would write nowhere; what changes the row order here is `ref`, in the same table's own column.
// A predictor raises three questions, and the table asks them side by side: how it becomes levels
// (`shape`), what its effect is measured AGAINST (`ref`), and in what unit that effect is read
// (`multiplier`). The last two are numeric-only, and a CUT number is a factor by the time the model
// sees it -- so it takes a factor's baseline vocabulary and loses the scaling entirely.

var TABX = {
    hint:    "padding:4px 6px 2px;opacity:0.65;font-style:italic;",
    refName: "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    refNote: "opacity:0.6;font-style:italic;",
    // ONE GREYSCALE LADDER for every box this panel draws, recessed -> raised, the same one the
    // shared per-variable table already uses (#E4E4E4 body / #CCCCCC head). Material reads
    // elevation as lightness in a light theme, so a WELL is darker than the CARDS sitting on it,
    // and each step is ~16 levels -- enough to separate at a glance, little enough not to stripe:
    //   #CCCCCC  a header row in the SHARED per-variable table (TABXV.head), whose heads label a
    //            grid of cells. ⚠ a head standing over CARDS takes no fill at all -- mtHead and
    //            crossHead sit on the well itself, because a filled bar there reads as one more
    //            row of the list rather than as its heading.
    //   #DCDCDC  a WELL: the box holding a list of rows and the "+ Add" button that grows it
    //   #ECECEC  a CARD raised on that well: one interaction row, one model, one outcome
    //   #FFFFFF  an INPUT: a <select>, a text box, a button. Reserved for them, and it is what
    //            makes them read as inputs inside a grey pane.
    // ⚠ keep them as literal hex, not rgba() overlays: an overlay's result depends on whatever it
    // sits on, so the same key rendered two shades in two containers.
    well:     "margin:2px 6px 2px;padding:6px;border:1px solid rgba(0,0,0,0.20);border-radius:4px;background:#DCDCDC;box-sizing:border-box;",
    // model cards (one per model), raised on the well
    cardBox:  "border:1px solid rgba(0,0,0,0.16);border-radius:4px;background:#ECECEC;margin:4px 0;padding:6px 8px;min-width:0;box-sizing:border-box;",
    cardHead: "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
    cardName: "flex:1 1 auto;min-width:0;box-sizing:border-box;padding:2px 6px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;font-weight:600;",
    cardDel:  "flex:0 0 auto;border:none;background:transparent;cursor:pointer;font-size:1.1em;line-height:1;color:rgba(0,0,0,0.55);padding:2px 6px;",
    cardVars: "display:flex;flex-wrap:wrap;align-items:center;gap:4px 14px;",
    cardChk:  "display:inline-flex;align-items:center;gap:3px;white-space:nowrap;cursor:pointer;",
    chkSep:   "opacity:0.35;",                 // between a card's plain predictors and its pairs
    // the heading above each well (Interactions / Models)
    sectionHead: "font-weight:700;color:#000;margin:16px 6px 0;",
    // The interaction rows: [var1] * [var2] [x], raised on the well, headed by what each column IS.
    // ⚠ crossRow and crossHead repeat the SAME grid-template-columns: edit them together or the
    // header drifts from the rows it names. ⚠ NEVER a width beside a horizontal margin (22g-iv);
    // the well supplies the inset, so a row takes none. The two selects are kept NARROW on purpose:
    // the options pane is ~340px at its narrowest, and anything wider clips the delete button off
    // the right edge -- which is also why xDel has `min-width:0` (a <button>'s intrinsic width
    // otherwise blows its own grid track open and overflows the row).
    crossRow: "display:grid;grid-template-columns:minmax(48px,1fr) 8px minmax(48px,1fr) 18px;align-items:center;gap:6px;min-width:0;box-sizing:border-box;padding:4px 6px;margin:4px 0;border:1px solid rgba(0,0,0,0.16);border-radius:4px;background:#ECECEC;",
    crossHead:"display:grid;grid-template-columns:minmax(48px,1fr) 8px minmax(48px,1fr) 18px;align-items:end;gap:6px;min-width:0;box-sizing:border-box;padding:2px 6px 0;margin:0;color:#000;font-weight:600;font-style:italic;line-height:1.15;",
    crossOp:  "font-weight:700;opacity:0.7;text-align:center;",
    // ⚠ min-width:0 + width:100%: a grid item defaults to min-width:auto, so a button's intrinsic
    // width widens its track and pushes the row past the pane. This is what made it unreachable.
    xDel:     "min-width:0;width:100%;border:none;background:transparent;cursor:pointer;font-size:1.05em;line-height:1;color:rgba(0,0,0,0.55);padding:0;",
    addBtn:   "margin:4px 0 0;padding:3px 10px;border:1px dashed rgba(0,0,0,0.35);border-radius:4px;background:#fff;color:#000;cursor:pointer;font-weight:600;",
    // Phase 15d: `white-space:nowrap` keeps the trials input and its suffix on ONE line.
    multWrap: "display:flex;align-items:center;gap:2px;min-width:0;white-space:nowrap;",
    // per-outcome Model table: [name] [family =] [link =] [outcome_level / trials]. Full width, a
    // stretching last column so long level labels stay readable. The first three columns are HEADED
    // (mtHead) with the argument names, so the panel teaches the two questions it asks per outcome;
    // the 4th is not, because which of outcome_level / trials it holds depends on the row.
    // ⚠ mtRow and mtHead repeat the SAME grid-template-columns: edit them together or the header
    // drifts from the rows it names.
    mtRow:   "display:grid;grid-template-columns:minmax(70px,1fr) 150px 145px 105px;align-items:center;gap:10px;min-width:0;box-sizing:border-box;padding:5px 8px;margin:4px 0;border:1px solid rgba(0,0,0,0.16);border-radius:4px;background:#ECECEC;",
    mtHead:  "display:grid;grid-template-columns:minmax(70px,1fr) 150px 145px 105px;align-items:end;gap:10px;min-width:0;box-sizing:border-box;padding:2px 8px 0;margin:0;color:#000;font-weight:600;",
    // the well the whole per-outcome table sits in -- the same material as every other list here.
    mtCard:  "margin:2px 0 6px;padding:6px;border:1px solid rgba(0,0,0,0.20);border-radius:4px;background:#DCDCDC;box-sizing:border-box;",
    mtSel:   "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    mtTrials:"width:70px;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;"
};

// `ref` on a still-numeric predictor is an ANCHOR -- the value its effect is read from. tab_reg()
// takes four keywords or a number; blank = the model's own default, which IS the mean.
// ⚠ so `"mean"` is NOT offered beside it: the two write different options and produce the same
// table, and a list reading "mean (default)" then "mean" only asks the user to tell them apart.
// The blank is the one kept, because it stores nothing and lets tab_reg() own its own default.
var REG_ANCHORS = ["", "median", "min", "max"];
var REG_ANCHOR_LABELS = { "": _("mean (default)") };

var VAR_TABLE_HOST = {
    ctrl: "varTableCtrl",
    cols: [
        { key: "name",   head: _("predictor"),        width: "minmax(90px,1fr)" },
        { key: "levels", head: _("levels / shape ="), width: "165px",
          tip: _("a factor shows how many levels it has; a number chooses how it is cut into groups") },
        { key: "ref",    head: _("ref = <i>(reference)</i>"), width: "180px",
          tip: _("the baseline each effect is measured against") },
        { key: "act",    head: _("multiplier ="),     width: "85px",
          tip: _("a number's effect is read per k units") }
    ],
    emptyHint: _("Select predictors to choose their baseline and shape."),
    mergeTip:  _("click to relevel"),
    closeTip:  _("click to close"),
    // Phase 22g-iv: the ▲/▼ bar DOES belong here. `tab_reg()` has no `levels_order` argument, so the
    // option drives a jmvtabreg-only prep step instead: jmvtab_reg_build() relevels the predictor
    // columns before the fit. That is also what makes the reference rule below true by construction.
    orderOpt:  "levels_order",
    boldFirst: true,
    isCut:     function (sh) { return TABX_SHAPES_CUT.indexOf(sh) >= 0; },

    groups: function (ui) {
        return [{ label: _("Predictors"), numericMayKeep: true,
                  vars: ui.predictors ? utils.clone(ui.predictors.value(), []) : [] }];
    },

    // ⚠ the predictor set and how a level is SPELT -- nothing this table writes (SHARED header).
    sig: function (ui) {
        return JSON.stringify([ui.predictors ? utils.clone(ui.predictors.value(), []) : [],
                               ui.cleannames ? ui.cleannames.value() : true]);
    },

    shapes: function () { return TABX_SHAPES; },

    reconcile: function (ui, all) {
        reconcileArr(ui, "ref_levels", all);
        reconcileArr(ui, "multiplier", all);
    },

    // ⚠ TWO invariants, and both are repaired here rather than guarded at every call site.
    // (1) A regression's baseline IS the first level, so after any reorder or merge the stored
    //     `ref` is re-read off the order -- which is what keeps the bold first level, the `ref =`
    //     cell and the fit saying one thing.
    // (2) A `shape` pick moves a predictor between two vocabularies, and a stored value from the
    //     wrong one is not a stale display but an ABORT: tab_reg() refuses `multiplier` on a factor
    //     (reg_check_continuous_names()), and an anchor keyword cannot name a level.
    varSync: function (ui, v, kind) {
        if (!kind.isNumber) {
            var lv = tabxvLevels(ui, VAR_TABLE_HOST, v, kind.cached);
            if (lv.length) arrWrite(ui, "ref_levels", v, "ref", lv[0]);
            arrWrite(ui, "multiplier", v, "k", "");
            return;
        }
        var stored = arrGet(ui, "ref_levels", v, "ref");
        if (kind.isCut) {
            arrWrite(ui, "multiplier", v, "k", "");
            if (["first", "last"].indexOf(stored) < 0) arrWrite(ui, "ref_levels", v, "ref", "");
        } else if (stored && REG_ANCHORS.indexOf(stored) < 0 && isNaN(Number(stored))) {
            arrWrite(ui, "ref_levels", v, "ref", "");
        }
    },

    refCell: function (ui, cell, v, kind) {
        if (kind.loading) return;
        var stored = arrGet(ui, "ref_levels", v, "ref");
        if (kind.isCut) {
            // The groups' LABELS are computed R-side from the data's own quantiles, so the only
            // baselines nameable here are the two positional keywords tab_reg() declares.
            cell.appendChild(makeSelect(TABXV.sel, ["first", "last"],
                                        { first: _("first group (lowest)"), last: _("last group (highest)") },
                                        (stored === "last") ? "last" : "first",
                                        function (r) { arrWrite(ui, "ref_levels", v, "ref", r); }));
            return;
        }
        if (kind.isNumber) {
            if (stored === "mean") stored = "";      // a synonym of the default -- show the one entry
            var anchors = REG_ANCHORS.slice();
            if (stored && anchors.indexOf(stored) < 0) anchors.push(stored);   // a typed number stays
            var a = makeSelect(TABXV.sel, anchors, REG_ANCHOR_LABELS, stored,
                               function (r) { arrWrite(ui, "ref_levels", v, "ref", r); });
            a.title = _("the value the effect of this number is read from");
            cell.appendChild(a);
            return;
        }
        // A FACTOR: the baseline is the FIRST level, so picking one here MOVES it to the front of
        // the order -- the same write the ▲/▼ bar makes. The two controls are two ways to say it.
        var levels = tabxvLevels(ui, VAR_TABLE_HOST, v, kind.cached);
        if (levels.length === 0) return;
        var sel = makeSelect(TABXV.sel, levels,
                             function (o) { return tabxvClean(ui, o); },
                             levels.indexOf(stored) >= 0 ? stored : levels[0],
                             function (r) { regRefToFirst(ui, v, kind, r); });
        cell.appendChild(sel);
    },

    // Phase 18z9: a TEXT input, because the scaling accepts the same three things tab_reg()'s
    // `multiplier` does -- "sd" (the default), "2sd", or a number of units. A number input could not
    // express the keywords, and the per-1-unit effect of a continuous predictor is usually too small
    // to colour at all. A factor (a cut number included) has nothing to scale.
    unitCell: function (ui, cell, v, kind) {
        if (kind.loading || kind.isCut) return;
        if (!ui.multiplier) return;
        var wrap = document.createElement("span"); wrap.style.cssText = TABXV.unit;
        var pre  = document.createElement("span"); pre.textContent = "×";
        var inp  = document.createElement("input");
        inp.type = "text"; inp.style.cssText = TABXV.inp;
        inp.placeholder = "2sd"; inp.value = arrGet(ui, "multiplier", v, "k");
        inp.title = _('the effect per k units: "2sd" (the default), "sd", or a number');
        inp.addEventListener("change", function () {
            arrWrite(ui, "multiplier", v, "k", inp.value);
        });
        wrap.appendChild(pre); wrap.appendChild(inp);
        cell.appendChild(wrap);
    }
};

// Move `r` to the front of `v`'s stored order and record it as the reference. The order is written
// in the RAW level names (it is what R relevels on), while `r` may be a MERGED run's label -- so a
// merged run moves as its whole run of source levels.
var regRefToFirst = function (ui, v, kind, r) {
    var natural = kind.cached || [];
    var order   = tabxvStoredOrder(ui, VAR_TABLE_HOST, v, natural);
    var groups  = tabxmGroups(ui, v);
    var runOf   = function (l) {
        for (var i = 0; i < groups.length; i++)
            if (groups[i].levels.indexOf(l) >= 0)
                return groups[i].label || tabxvRunLabel(groups[i].levels);
        return l;
    };
    var head = order.filter(function (l) { return runOf(l) === r; });
    if (head.length === 0) { arrWrite(ui, "ref_levels", v, "ref", r); return; }
    var rest = order.filter(function (l) { return runOf(l) !== r; });
    tabxvWriteOrder(ui, VAR_TABLE_HOST, v, head.concat(rest));
    arrWrite(ui, "ref_levels", v, "ref", r);
    tabxvRebuildList(ui, VAR_TABLE_HOST, v);   // the open list must show the order it just got
    tabxvRefreshVar(ui, VAR_TABLE_HOST, v);
};

var renderVarTable = function (ui) { tabxvRender(ui, VAR_TABLE_HOST); };

// ---- Per-outcome Model table CustomControl (modelTableCtrl) ------------------------------
// One row per DEPENDENT = [name] [family select filtered by the outcome's R type] [col-3]. col-3 is
// the binomial MODELLED level (for a 2-level factor, default the FIRST level = the modelled/success
// level) or the number of TRIALS (for a numeric binomial outcome; blank -> the observed max). Stored in
// the hidden family / outcome_level / trials arrays, folded by jmvtab_reg_* into tab_reg(family /
// inverse_two_level_factors / trials). Phase h: the family is DETECTED client-side and stored as an
// explicit concrete pick (no "auto" default), so the backend never re-detects. Mirrors the refPicker's
// async column fetch (own cache: it needs measureType + dataType, which the refPicker's cache drops).

var mtCache = {};          // var -> {mt: measureType, dataType, levels: [labels]|null} | FETCHING
var lastModelSig = null;

// The picker rules, READ off the generated tables above -- no rule is written twice. No "auto
// (detected)" row: the family is detected client-side and pre-selected as a CONCRETE choice, so the
// backend never re-detects. quasipoisson is deliberately not offered (an unweighted poisson already
// scales its SEs for over-dispersion); it stays available in the R API.
var familyLabelsFor = function(c) {
    return (c && c.levels !== null && c.levels.length === 2) ? TABX_FAMILY_LABEL_BINARY : TABX_FAMILY_LABEL;
};

// The outcome KIND, the one fact both sides can compute from a column alone (reg_outcome_kind).
var outcomeKind = function(c) {
    if (!c || c.levels === null)   return "numeric";
    if (c.levels.length === 2)     return "binary";
    if (c.mt === "ordinal")        return "ordered";
    return "nominal";
};
var detectFamily     = function(c) { return TABX_OUTCOME_DETECT[outcomeKind(c)]; };
var familyOptionsFor = function(c) { return TABX_OUTCOME_OFFERS[outcomeKind(c)]; };

var modelTableSig = function(ui) {
    var deps = utils.clone(ui.outcome.value(), []);
    var fams = ui.family ? utils.clone(ui.family.value(), []) : [];
    return JSON.stringify([deps, fams]);       // families included so col-3 re-renders on a family flip
};

var afterFetchMT = function(ui) {
    if (ui.modelTableCtrl && ui.modelTableCtrl.$el) renderModelTable(ui);
};

// arrGet / arrWrite / reconcileArr / makeSelect now live in the SHARED block above --
// the per-variable table needs the same three verbs over the same {var, <key>} option shape.

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
    var storedF  = arrGet(ui, "family", v, "family");
    var famSel   = (storedF && opts.indexOf(storedF) >= 0) ? storedF : detected;
    if (!storedF) arrWrite(ui, "family", v, "family", detected);   // persist the detected default
    var famSelEl = makeSelect(TABX.mtSel, opts, familyLabelsFor(c), famSel,
        function(f) { arrWrite(ui, "family", v, "family", f); renderModelTable(ui); });
    if (opts.length <= 1) famSelEl.disabled = true;
    row.appendChild(famSelEl);

    // The LINK: which measure this outcome's model estimates -- the only choice that changes the
    // model. The drop-down offers exactly TABX_LINKS[family], so it can never claim a fit the family
    // has no arm for, and a stored pick the newly-chosen family cannot fit is cleared rather than
    // left to abort R-side (the same self-healing the family select does against familyOptionsFor).
    var links    = TABX_LINKS[famSel] || ["auto"];
    var storedLk = arrGet(ui, "link", v, "link");
    if (storedLk && links.indexOf(storedLk) < 0) { arrWrite(ui, "link", v, "link", ""); storedLk = ""; }
    row.appendChild(makeSelect(TABX.mtSel, links, TABX_LINK_LABEL, storedLk || "auto",
        function(lk) {
            arrWrite(ui, "link", v, "link", lk === "auto" ? "" : lk);
            applyModelEnables(ui);            // the measure/effect radios ask THIS outcome's link
        }));

    // col-4: an outcome level where the FAMILY takes one; a numeric outcome set to binomial -> trials.
    // ⚠ the gate is TABX_OUTCOME_LEVEL_ROLE, i.e. REG_FAMILIES$<f>$outcome_level, NOT "the outcome
    // has exactly 2 levels" -- which hid the picker on both families that most need it: a binomial
    // on a 3+ level outcome (one level against the rest) and a multinomial (whose level is the
    // BASELINE everything is read against). The role also says which of the two it is.
    var role        = TABX_OUTCOME_LEVEL_ROLE[famSel];
    var hasLevels   = c.levels && c.levels.length >= 2;
    var isNumBinom  = (c.levels === null) && (famSel === "binomial");
    if (role && hasLevels) {
        var storedL = arrGet(ui, "outcome_level", v, "level");
        var selL = (storedL && c.levels.indexOf(storedL) >= 0) ? storedL : c.levels[0];  // default first
        var lvSel = makeSelect(TABX.mtSel, c.levels,
            function(l) { return tabxvClean(ui, l); }, selL,
            function(l) { arrWrite(ui, "outcome_level", v, "level", l === c.levels[0] ? "" : l); });
        lvSel.title = (role === "modelled")
            ? _("outcome_level = the level modelled, against every other one merged")
            : _("outcome_level = the baseline category the others are read against");
        row.appendChild(lvSel);
    } else if (isNumBinom) {
        var wrapT = document.createElement("div"); wrapT.style.cssText = TABX.multWrap;
        var inp = document.createElement("input");
        inp.type = "number"; inp.step = "1"; inp.min = "1"; inp.style.cssText = TABX.mtTrials;
        inp.placeholder = "max"; inp.value = arrGet(ui, "trials", v, "n");
        inp.addEventListener("change", function() { arrWrite(ui, "trials", v, "n", inp.value); });
        // ⚠ the space is OUTSIDE `_()`: jamovi looks a msgid up trimmed but the catalogue stores it
        // as written, so edge whitespace inside the call is a permanent silent miss.
        var sufT = document.createElement("span"); sufT.style.cssText = TABX.refNote;
        sufT.textContent = " " + _("trials");
        wrapT.appendChild(inp); wrapT.appendChild(sufT);
        row.appendChild(wrapT);
    } else {
        row.appendChild(document.createElement("span"));       // keep the 4-column grid aligned
    }
    frag.appendChild(row);
};

var renderModelTable = function(ui) {
    if (!ui.modelTableCtrl || !ui.outcome) return;
    lastModelSig = modelTableSig(ui);
    var deps = utils.clone(ui.outcome.value(), []);
    reconcileArr(ui, "family", deps);
    reconcileArr(ui, "link", deps);
    reconcileArr(ui, "outcome_level", deps);
    reconcileArr(ui, "trials", deps);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-model", "1");
    frag.style.cssText = TABX.mtCard;
    if (deps.length === 0) {
        var hint = document.createElement("div"); hint.style.cssText = TABX.hint;
        hint.textContent =
            _("Add one or more outcome variables to choose each one's model family and link.");
        frag.appendChild(hint);
    } else {
        // The header names the two ARGUMENTS the table sets, so a user learns them by clicking --
        // the same reason every other control in this panel is labelled `<argument> =`.
        var head = document.createElement("div"); head.style.cssText = TABX.mtHead;
        [_("outcome"), "family =", "link ="].forEach(function(t) {
            var h = document.createElement("span"); h.textContent = t; head.appendChild(h);
        });
        frag.appendChild(head);
        deps.forEach(function(v) { renderModelRow(ui, frag, v); });
    }
    var root = ui.modelTableCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
    applyModelEnables(ui);
};

// The (family, link) pair of each currently selected outcome -- read off the Model table, skipping
// the outcomes whose column metadata is still being fetched. Everything the Model box greys out is a
// question about THESE, and since Phase 22g-i each outcome answers it with its OWN link.
var selectedFamilyLinks = function(ui) {
    var out = [];
    if (!ui.outcome) return out;
    var deps = utils.clone(ui.outcome.value(), []);
    for (var i = 0; i < deps.length; i++) {
        var c = mtCache[deps[i]];
        if (!c || c === FETCHING) continue;
        var f  = arrGet(ui, "family", deps[i], "family");
        f = (f && TABX_FAMILY_LABEL[f]) ? f : detectFamily(c);
        var lk = arrGet(ui, "link", deps[i], "link");
        var ok = TABX_LINKS[f] || ["auto"];
        out.push({ family: f, link: (lk && ok.indexOf(lk) >= 0) ? lk : "auto" });
    }
    return out;
};

// WHICH (effect x measure) combinations the selected outcomes offer, straight off the generated grid
// -- so the UI cannot claim a combination tab_reg() refuses, nor grey one it accepts. The LINK axis
// is no longer a question asked here: the table's own drop-down lists exactly TABX_LINKS[family], so
// a link that cannot be fitted is unreachable rather than greyed.
var measureOffered = function(ui, effect, measure) {
    var pairs = selectedFamilyLinks(ui);
    if (pairs.length === 0) return true;                 // nothing selected yet -> leave enabled
    for (var i = 0; i < pairs.length; i++) {
        var g = TABX_ESTIMANDS[pairs[i].family];
        if (!g) return false;
        g = g[pairs[i].link];
        if (!g || !g[effect] || g[effect].indexOf(measure) < 0) return false;  // ALL must offer it
    }
    return true;
};

// ⚠ radio NAME -> the value it sets, and it must follow the .u.yaml's ORDER, not just its values:
// a re-order or a value rename moves every pair here, and a stale entry greys the WRONG button,
// silently. test-jamovi-vocabulary.R walks this map against the .u.yaml for exactly that reason.
var MEASURE_OF_RADIO = { measure_1: "auto", measure_2: "difference", measure_3: "ratio",
                         measure_4: "odds_ratio", measure_5: "raw_coefficient" };

// `color = "between_groups"` reads each group's effect against the first group's, so with no
// tab_vars there are no groups. A Variables array's length is invisible to the declarative `enable:`
// DSL (an empty array is truthy), which is why this one is imperative while `adjustment`'s gate on
// `empirical` is a plain `enable: (empirical)` in the .u.yaml.
var applyVarEnables = function(ui) {
    var tv = ui.tab_vars ? ui.tab_vars.value() : null;
    if (ui.color_3) ui.color_3.setEnabled(!!(tv && tv.length > 0));
};

// Only `measure` is greyed here. `effect` became a ComboBox, and a ComboBox cannot grey one of its
// own items -- the same price `display` paid in 22g-iii; a combination the outcomes do not offer is
// refused R-side, by name, with its cures listed.
var applyModelEnables = function(ui) {
    var eff = ui.effect ? ui.effect.value() : "auto";
    Object.keys(MEASURE_OF_RADIO).forEach(function(nm) {
        if (ui[nm] && ui[nm].setEnabled)
            ui[nm].setEnabled(measureOffered(ui, eff, MEASURE_OF_RADIO[nm]));
    });
};

// ---- Interactions + model-comparison builder CustomControl (modelBuilderCtrl) ------------
// ONE control, two halves, in the order a model is built:
//
//   Interactions   a row per pair = [var1] x [var2] [x delete], plus "+ Add interaction". An
//                  interaction is a PREDICTOR whose levels are combinations, so tab_reg() takes it
//                  INSIDE `predictors` as the key `a*b`; the rows are the hidden `crosses` array,
//                  and jmvtab_reg_cross_keys() turns them into those keys. The FIRST variable is
//                  the modified one -- the grammar's own reading of `a*b`.
//   Models         a card per model = an editable name + a checkbox per pool variable AND per
//                  defined interaction + a delete button; "+ Add model" appends a card holding the
//                  full pool and NO interaction. Cards are the hidden `models` Array
//                  (Group{label, vars, crosses}); jmvtab_reg_models() folds them into `predictors`
//                  (EMPTY builder -> the flat pool, every pair applied = one model; >=1 card -> a
//                  named list = model comparison, TESTED automatically since 22g-ii -- sequential
//                  where the models nest, against the first otherwise, so there is no picker).
//
// ⚠ Phase 22g-viii: a pair is DEFINED once and TICKED per model. Before that, jmvtab_reg_cross_fold()
// replaced both parents in EVERY card that held them, so an additive model could not be asked for
// beside a crossed one -- which is the one comparison defining an interaction exists for.
//
// ⚠ THREE things the picker cannot express, all made UNREACHABLE rather than repaired afterwards --
// the rule that deleted linkOffered() in 22g-i. `syncCrosses()` and `jmvtab_reg_cross_keys()` keep
// their own guards behind them, because a pair stored by an older build must not reach R either.
//   `a*a`         meaningless: picking the variable already on the other side SWAPS the pair
//   `a*b` + `b*c` a three-way interaction, which tabxplor does not fit -- so a variable another row
//                 already uses simply is not offered (crossClaimed)
//   `race*age`    where `age` is the continuous one: reg_cross_resolve() reads it as `age*race`,
//                 so crossOrder() puts the pair in that order here (crossOrder)
//
// The signature EXCLUDES `models` and `crosses` (as the per-variable table excludes ref_levels): a
// name edit and a tick write them and are SKIPPED by `updated`, so the in-place DOM edit stands. It
// DOES carry the outcome count, because a second outcome closes the door on a second card (below).
// ⚠ add / delete / a <select> pick change what the OTHER half shows -- the card chips' labels, the
// row's own options -- so those three re-render synchronously in their handlers. A tick and a name
// keystroke must NOT: they repaint in place, or the edit that caused the rebuild is clobbered.

var lastModelsSig = null;

var modelsSig = function(ui) {
    var pool = utils.clone(ui.predictors.value(), []);
    var deps = ui.outcome ? utils.clone(ui.outcome.value(), []) : [];
    return JSON.stringify([pool, deps.length]);
};

// ---- the interactions half ---------------------------------------------------------------

var crossesGet = function(ui) { return utils.clone(ui.crosses.value(), []); };

// ⚠ `a*b` is `tab_reg()`'s OWN spelling -- the key it takes inside `predictors`, and the name the
// var column, the footer rows and reg_formulas() all print. It is what a card's tick-box is
// labelled with, so a jamovi user reads the R syntax rather than a prettified twin of it.
var crossKey = function(e) { return e.var1 + "*" + e.var2; };

// What reg_cross_resolve() will see: a column is CATEGORICAL if it is a factor, or if its `shape`
// cuts it into groups. `null` while the column is still being fetched -- an unknown kind must not
// move anything.
var crossKind = function(ui, v) {
    var cached = cachedLevels(v);
    if (cached === undefined) return null;
    if (cached !== null) return "factor";
    var sh = arrGet(ui, "shape", v, "shape");
    return (sh && TABX_SHAPES_CUT.indexOf(sh) >= 0) ? "factor" : "numeric";
};

// ⚠ THE SWAP, mirroring reg_cross_resolve(). `*` is symmetric in the MODEL -- `a*b` and `b*a` are
// one fit -- but the ROWS are about the FIRST variable, and only a continuous one has slopes to
// show within groups. So R silently reads `race*age` as `age*race`, and the picker would otherwise
// keep saying "effect of race within each level of age" about a table that says the opposite.
// Applied where the two column heads already explain which is which, so it needs no message.
var crossOrder = function(ui, e) {
    return (crossKind(ui, e.var1) === "factor" && crossKind(ui, e.var2) === "numeric")
        ? { var1: e.var2, var2: e.var1 } : e;
};

// The parents a set of keys names -- the mirror of jmvtab_reg_cross_parents(): a pair SUPPLIES both,
// so a card holding the key must not also hold them.
var crossParents = function(keys) {
    var out = [];
    keys.forEach(function(k) {
        k.split("*").forEach(function(p) { if (out.indexOf(p) < 0) out.push(p); });
    });
    return out;
};

// The one sweep over `crosses`: drop rows whose variables have left the pool, that name the same
// variable twice, or that reuse a variable an earlier row already took (one interaction per
// variable -- see crossClaimed) -- then put every survivor in the order R will read it.
// Returns { keys, renames }: `renames` is old-key -> new-key for the pairs the swap moved, because
// a card stores the KEY it ticked and must follow it rather than silently lose the interaction.
// Guarded: an unchanged array is not re-set.
var syncCrosses = function(ui, pool) {
    var cur = crossesGet(ui), used = {}, kept = [], renames = {};
    cur.forEach(function(e) {
        if (!e || e.var1 === e.var2) return;
        if (pool.indexOf(e.var1) < 0 || pool.indexOf(e.var2) < 0) return;
        if (used[e.var1] || used[e.var2]) return;
        var was = crossKey(e), now = crossOrder(ui, e);
        used[now.var1] = true; used[now.var2] = true;
        if (crossKey(now) !== was) renames[was] = crossKey(now);
        kept.push(now);
    });
    if (JSON.stringify(kept) !== JSON.stringify(cur)) ui.crosses.setValue(kept);
    return { keys: kept.map(crossKey), renames: renames };
};

// What picking `v` on one side of row `e` yields. Picking the variable already on the other side
// SWAPS the pair instead of making `a*a`.
var crossPick = function(e, side, v) {
    if ((side === "var1" ? e.var2 : e.var1) === v) return { var1: e.var2, var2: e.var1 };
    return (side === "var1") ? { var1: v, var2: e.var2 } : { var1: e.var1, var2: v };
};

// ⚠ ONE INTERACTION PER VARIABLE. `a*b` beside `b*c` is a three-way interaction, which tabxplor
// does not fit -- the second pair would simply not apply. So a variable another row already uses is
// unreachable here, rather than pickable and then silently dropped. `exceptI` is the row asking.
var crossClaimed = function(ui, exceptI) {
    var out = {};
    crossesGet(ui).forEach(function(e, j) {
        if (j === exceptI || !e) return;
        out[e.var1] = true; out[e.var2] = true;
    });
    return out;
};

// What either side of row `i` may offer: everything not claimed by ANOTHER row -- the other side of
// THIS row included, because picking it swaps the pair rather than repeating a variable.
var crossOptions = function(ui, i, pool) {
    var claimed = crossClaimed(ui, i);
    return pool.filter(function(v) { return !claimed[v]; });
};

// The first pair of variables no row uses -- so "+ Add" cannot seed a variable that is spoken for.
var crossFreePair = function(ui, pool) {
    var claimed = crossClaimed(ui, -1);
    var free = pool.filter(function(v) { return !claimed[v]; });
    return (free.length < 2) ? null : { var1: free[0], var2: free[1] };
};

var setCrossVar = function(ui, i, side, val) {
    var arr = crossesGet(ui);
    if (!arr[i]) return;
    arr[i] = crossPick(arr[i], side, val);
    ui.crosses.setValue(arr);
    renderModelBuilder(ui);        // syncCrosses() then orders it; the cards' labels follow
};

var addCross = function(ui, pool) {
    var e = crossFreePair(ui, pool);
    if (!e) return;
    var arr = crossesGet(ui);
    arr.push(e);
    ui.crosses.setValue(arr);
    renderModelBuilder(ui);
};

var deleteCross = function(ui, i) {
    var arr = crossesGet(ui);
    arr.splice(i, 1);
    ui.crosses.setValue(arr);
    renderModelBuilder(ui);        // the cards lose that chip (reconcileModels sweeps it)
};

var renderCrossRow = function(ui, frag, e, i, pool) {
    var row = document.createElement("div"); row.style.cssText = TABX.crossRow;
    row.appendChild(makeSelect(TABX.mtSel, crossOptions(ui, i, pool), null, e.var1,
        function(v) { setCrossVar(ui, i, "var1", v); }));
    // the operator is `*`, the spelling `predictors` itself takes -- the row teaches the R syntax
    var x = document.createElement("span"); x.style.cssText = TABX.crossOp; x.textContent = "*";
    row.appendChild(x);
    row.appendChild(makeSelect(TABX.mtSel, crossOptions(ui, i, pool), null, e.var2,
        function(v) { setCrossVar(ui, i, "var2", v); }));
    var del = document.createElement("button");
    del.type = "button"; del.style.cssText = TABX.xDel; del.textContent = "×";
    del.title = _("Remove this interaction");
    del.addEventListener("click", function() { deleteCross(ui, i); });
    row.appendChild(del);
    frag.appendChild(row);
};

var renderCrossBlock = function(ui, frag, pool, keys) {
    var h = document.createElement("div"); h.style.cssText = TABX.sectionHead;
    h.textContent = _("Interactions");
    frag.appendChild(h);

    if (pool.length < 2) {
        var h0 = document.createElement("div"); h0.style.cssText = TABX.hint;
        h0.textContent = _("Select at least two predictors: an interaction crosses two of them.");
        frag.appendChild(h0);
        return;
    }

    // one WELL holding the rows AND the button that grows them, so a list reads as one object
    var well = document.createElement("div"); well.style.cssText = TABX.well;
    if (keys.length > 0) {
        var head = document.createElement("div"); head.style.cssText = TABX.crossHead;
        [_("effect of"), "", _("within each level of"), ""].forEach(function(t) {
            var s = document.createElement("span"); s.textContent = t; head.appendChild(s);
        });
        well.appendChild(head);
        crossesGet(ui).forEach(function(e, i) { renderCrossRow(ui, well, e, i, pool); });
    }
    var add = document.createElement("button");
    add.type = "button"; add.style.cssText = TABX.addBtn; add.textContent = _("+ Add interaction");
    if (crossFreePair(ui, pool) === null) { add.disabled = true; add.style.opacity = "0.45"; }
    else add.addEventListener("click", function() { addCross(ui, pool); });
    well.appendChild(add);
    frag.appendChild(well);

    // Only the EMPTY state gets a line, and only because there is nothing else to read there: with
    // rows on screen the two column heads already say what the pair means.
    if (keys.length === 0) {
        var note = document.createElement("div"); note.style.cssText = TABX.hint;
        note.textContent = _("Cross two predictors to read the effect of the first within each level of the second.");
        frag.appendChild(note);
    }
};

// ---- the models half ----------------------------------------------------------------------

var modelsGet = function(ui) { return utils.clone(ui.models.value(), []); };

// Store card `i` in POOL order then KEY order (dropping anything neither offers). Guarded setValue.
var setCard = function(ui, i, vars, crs, pool, keys) {
    var arr = modelsGet(ui);
    if (!arr[i]) return;
    arr[i] = { label:   arr[i].label || "",
               vars:    pool.filter(function(v) { return vars.indexOf(v) >= 0; }),
               crosses: keys.filter(function(k) { return crs.indexOf(k)  >= 0; }) };
    ui.models.setValue(arr);
};

var setCardLabel = function(ui, i, label) {
    var arr = modelsGet(ui);
    if (!arr[i]) return;
    arr[i] = { label: label, vars: (arr[i].vars || []).slice(),
               crosses: (arr[i].crosses || []).slice() };
    ui.models.setValue(arr);
};

// A new card holds the full pool and NO interaction: the additive model is the one a with/without
// comparison starts from, and it is what the panel showed before any card existed.
var addCard = function(ui, pool) {
    var arr = modelsGet(ui);
    arr.push({ label: "", vars: pool.slice(), crosses: [] });
    ui.models.setValue(arr);
    renderModelBuilder(ui);                              // count changed -> synchronous re-render
};

var deleteCard = function(ui, i) {
    var arr = modelsGet(ui);
    arr.splice(i, 1);
    ui.models.setValue(arr);
    renderModelBuilder(ui);
};

// Drop vars no longer in the pool, keys no longer defined, and any parent of a key the card still
// holds; keep the card + its name (a growing pool does NOT retro-add a new predictor to existing
// cards -- correct model-comparison semantics). ⚠ `renames` first: a card stores the KEY it ticked,
// so when syncCrosses() puts a pair in R's own order the card must FOLLOW it rather than find its
// key undefined and silently drop the interaction. ⚠ the kept sets are SUBSETS, so comparing
// lengths is comparing contents. Guarded setValue.
var reconcileModels = function(ui, pool, keys, renames) {
    var arr = modelsGet(ui), changed = false;
    for (var i = 0; i < arr.length; i++) {
        var vars = arr[i].vars || [];
        var crs  = (arr[i].crosses || []).map(function(k) { return renames[k] || k; });
        var keptK = keys.filter(function(k) { return crs.indexOf(k) >= 0; });
        var par   = crossParents(keptK);
        var keptV = pool.filter(function(v) {
            return vars.indexOf(v) >= 0 && par.indexOf(v) < 0;
        });
        if (keptV.length !== vars.length || keptK.length !== crs.length) {
            arr[i] = { label: arr[i].label || "", vars: keptV, crosses: keptK };
            changed = true;
        }
    }
    if (changed) ui.models.setValue(arr);
};

var renderModelCard = function(ui, frag, card, i, pool, keys) {
    var box  = document.createElement("div"); box.style.cssText = TABX.cardBox;
    var head = document.createElement("div"); head.style.cssText = TABX.cardHead;

    var name = document.createElement("input");
    name.type = "text"; name.style.cssText = TABX.cardName;
    name.placeholder = _("model") + (i + 1); name.value = card.label || "";
    name.addEventListener("input", function() { setCardLabel(ui, i, name.value); });
    head.appendChild(name);

    var del = document.createElement("button");
    del.type = "button"; del.style.cssText = TABX.cardDel; del.textContent = "×";
    del.title = _("Remove this model");
    del.addEventListener("click", function() { deleteCard(ui, i); });
    head.appendChild(del);
    box.appendChild(head);

    var vbox = document.createElement("div"); vbox.style.cssText = TABX.cardVars;
    var checks = [];
    var onNow  = {};
    (card.vars || []).forEach(function(v) { onNow[v] = true; });
    (card.crosses || []).forEach(function(k) { onNow[k] = true; });

    // A ticked interaction OWNS its two variables: they are unticked and greyed, and come back when
    // it is unticked. `locked` is what remembers which ones to restore, so the DOM alone carries it.
    var relock = function(first) {
        var by = {};
        checks.forEach(function(c) {
            if (!c.key || !c.cb.checked) return;
            c.v.split("*").forEach(function(p) { by[p] = c.v; });
        });
        checks.forEach(function(c) {
            if (c.key) return;
            var owner = by[c.v] || null;
            if (owner) c.cb.checked = false;
            else if (!first && c.locked) c.cb.checked = true;
            c.cb.disabled = !!owner;
            c.lab.style.opacity = owner ? "0.5" : "";
            c.lab.title = owner ? _("included in {pair}").replace("{pair}", owner) : "";
            c.locked = !!owner;
        });
    };

    var commit = function(cb) {
        relock(false);
        var vars = [], crs = [];
        checks.forEach(function(c) { if (c.cb.checked) (c.key ? crs : vars).push(c.v); });
        if (vars.length + crs.length === 0) { cb.checked = true; relock(false); return; }
        setCard(ui, i, vars, crs, pool, keys);
    };

    var addCheck = function(v, text, isKey) {
        var lab = document.createElement("label"); lab.style.cssText = TABX.cardChk;
        var cb  = document.createElement("input"); cb.type = "checkbox"; cb.checked = !!onNow[v];
        cb.addEventListener("change", function() { commit(cb); });
        checks.push({ v: v, cb: cb, lab: lab, key: isKey, locked: false });
        lab.appendChild(cb); lab.appendChild(document.createTextNode(" " + text));
        vbox.appendChild(lab);
    };

    pool.forEach(function(v) { addCheck(v, v, false); });
    if (keys.length > 0) {
        var sep = document.createElement("span"); sep.style.cssText = TABX.chkSep;
        sep.textContent = "│";
        vbox.appendChild(sep);
        // labelled with the KEY itself: a jamovi user reads `age*race`, the R spelling
        keys.forEach(function(k) { addCheck(k, k, true); });
    }
    relock(true);

    box.appendChild(vbox);
    frag.appendChild(box);
};

var renderCardsBlock = function(ui, frag, pool, keys) {
    var h = document.createElement("div"); h.style.cssText = TABX.sectionHead;
    h.textContent = _("Models comparison: choose predictors subsets");
    frag.appendChild(h);

    var cards = modelsGet(ui);
    // ⚠ A comparison tests two models OF THE SAME OUTCOME, so a SECOND card is refused while a
    // second outcome is selected -- and only then. One card and several outcomes is a plain
    // per-outcome table (jmvtab_reg_models() flattens it), which is why the door closes on the
    // second card rather than on the second outcome.
    var nDeps  = ui.outcome ? utils.clone(ui.outcome.value(), []).length : 0;
    var canAdd = (nDeps <= 1) || cards.length < 1;

    // one WELL holding the cards AND the button that grows them, as the interactions list has
    var well = document.createElement("div"); well.style.cssText = TABX.well;
    cards.forEach(function(card, i) { renderModelCard(ui, well, card, i, pool, keys); });
    var add = document.createElement("button");
    add.type = "button"; add.style.cssText = TABX.addBtn; add.textContent = _("+ Add model");
    add.disabled = !canAdd;
    if (!canAdd) add.style.opacity = "0.45";
    else add.addEventListener("click", function() { addCard(ui, pool); });
    well.appendChild(add);
    frag.appendChild(well);

    var note = document.createElement("div"); note.style.cssText = TABX.hint;
    note.textContent = !canAdd
        ? _("One predictor list only: a comparison tests two models of the SAME outcome, and several outcomes are selected.")
        : (cards.length === 0
           ? _("Add two or more models to compare them; leave empty to fit the full model.")
           : _("Each model draws from the predictors above; untick to leave one out."));
    frag.appendChild(note);
};

var renderModelBuilder = function(ui) {
    if (!ui.modelBuilderCtrl || !ui.models || !ui.predictors || !ui.crosses) return;
    lastModelsSig = modelsSig(ui);
    var pool = utils.clone(ui.predictors.value(), []);
    var cr   = syncCrosses(ui, pool);        // filter + order the pairs; `renames` the cards follow
    var keys = cr.keys;
    reconcileModels(ui, pool, keys, cr.renames);
    forceNaForCompare(ui);          // >=2 cards -> the models must share one complete-case population

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-models", "1");

    if (pool.length === 0) {
        var h0 = document.createElement("div"); h0.style.cssText = TABX.hint;
        h0.textContent = _("Select predictors first: they form the pool each model draws from.");
        frag.appendChild(h0);
    } else {
        renderCrossBlock(ui, frag, pool, keys);
        renderCardsBlock(ui, frag, pool, keys);
    }

    var root = ui.modelBuilderCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

module.exports = {

    // Root view update. Bound via `events: update:`; `view_updated` is the jus-3.0 alias -- keep both.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box changed: every per-variable widget lists them, so all three rebuild -- and
    // since 22g-viii the interactions ride inside the builder, so nothing is left out of this list.
    onChange_vars: function(ui) {
        applyVarEnables(ui);
        renderModelTable(ui);
        renderVarTable(ui);
        renderModelBuilder(ui);
    },

    // modelBuilderCtrl: build on create. On `updated`, re-render ONLY when the pool / outcome count
    // changed OR jamovi replaced our $el subtree (marker gone) -- a card name, a tick, an
    // interaction pick all write `models` / `crosses`, which are NOT in the signature, so they are
    // SKIPPED and whatever the handler painted stands.
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

    // varTableCtrl: the per-variable table (levels / cut / reference / scaling). Build on create;
    // on `updated`, rebuild only when the PREDICTOR SET changed or jamovi replaced our subtree --
    // a tick, a move, a `shape` pick or a reference pick writes an option that is deliberately
    // outside the signature, so the in-place repaint stands (see the SHARED header).
    varTableCtrl_creating: function(ui) { renderVarTable(ui); },
    varTableCtrl_updated:  function(ui) { tabxvUpdated(ui, VAR_TABLE_HOST); },

    // modelTableCtrl: build on create. On `updated`, re-render ONLY when the outcome set / chosen
    // families changed OR jamovi replaced our $el subtree -- a family / level / trials pick writes the
    // hidden family/outcome_level/trials (a family flip IS in the signature, so col-3 repaints),
    // so an in-place edit that keeps the signature is SKIPPED and the DOM edit stands.
    modelTableCtrl_creating: function(ui) { renderModelTable(ui); },
    modelTableCtrl_updated:  function(ui) {
        if (!ui.modelTableCtrl || !ui.outcome) return;
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
            // ⚠ `_()`: jamovi TRANSLATES an option's declared default, so without it the box opens
            // on the translated name and this button resets it to the English one.
            if (ui.export_dir)      ui.export_dir.setValue(_("~/Documents"));
            if (ui.export_filename) ui.export_filename.setValue(_("Reg_model"));   // == the .a.yaml default
            ui.resetPath.setValue(false);
        }
    }

};
