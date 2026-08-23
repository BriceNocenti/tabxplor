// Custom UI events for the jmvtab (Crosstables) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep
// it lean. See dev/tabxplor_2.0.0_jamovi_dev.md (§12 ref picker, §14 export) for the events API.
// jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

// --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---
// Generated from R/fmt_class.R (MEASURES), R/tab-display.R (DISPLAY_TOKENS) and
// R/var-shape.R (VAR_SHAPES). Re-run dev/generate_jamovi_js.R after changing them;
// the suite checks this block (test-jamovi-vocabulary.R).
var TABX_MEASURE_ODDS_RATIO = "odds_ratio";
var TABX_DISPLAY_ODDS_RATIO_FIELDS = ["or"];
var TABX_SHAPES_INDEX = ["auto", "levels", "median", "terciles", "quartiles", "quintiles", "deciles", "sd_bands"];
var TABX_SHAPES_COL = ["linear", "log", "sqrt", "levels", "median", "terciles", "quartiles", "quintiles", "deciles", "sd_bands"];
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

// Grey the controls that are meaningless without tab_vars (subtables): the total-table type and the
// comp reference-table choice. tab_vars is a Variables array, so its emptiness cannot be expressed in
// the declarative `enable:` DSL -> imperative setEnabled, re-run from onUpdate + onChange_vars (both
// fire on every variable change). The value is preserved (the backend forces totaltab="no"/comp="tab"
// with no tab_vars anyway), so a control returns to its stored value when tab_vars is re-added.
// Phase 15c: comp is now a single ComboBox (was comp_1 / comp_2 radios).
var applyVarEnables = function(ui) {
    var tv = ui.tab_vars ? ui.tab_vars.value() : null;
    var hasTab = !!(tv && tv.length > 0);
    ["totaltab_1", "totaltab_2", "totaltab_3", "comp"].forEach(function(nm) {
        if (ui[nm]) ui[nm].setEnabled(hasTab);
    });
};

// Phase 15c: the `subtext` note is a full-width, auto-grow <textarea> (a CustomControl driving the
// hidden `subtext` String option). Built once; on `updated` the option value is only re-synced into
// the textarea when they diverge (e.g. after the export "Reset" or a load) so typing is never
// clobbered. setValue is on `change` (blur), NOT every keystroke -> no re-run per character.
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

// WHY the path boxes ignored their 3fr/2fr stretch: jamovi compiles every grid cell to
// `minmax(max-content, <stretch>fr)`, so a control claims its min-content width BEFORE the stretch
// factors divide the row. A `width: largest` TextBox has a ~200px min (class silky-option-largest-text),
// so both boxes bottom out at ~200px and the fr ratio only splits the little that is left -- which is
// why folder and filename looked equal and the ".ext" was pushed off the right edge. Collapse that floor
// to 0 (min-width:0; width:100%) and the stretch factors govern the widths as expected. Done with a
// persistent <style> because jamovi re-renders controls and drops INLINE styles (the reason the old
// per-onUpdate inline widening never stuck). Only the two export boxes use `width: largest`, so the
// selector is effectively scoped to them.
// Phase 18r: a blank line at the bottom INSIDE each EXPANDED collapse box (compact when collapsed).
// The live jamovi box is `.jmv-collapse-view`, collapsed state `.view-colapsed` (jamovi's spelling) --
// confirmed against dev/jamovi/dev_console_live_capture/.../analysisui-*.css. The former guessed classes
// (.silky-options-collapse-box*, .jmv-options-collapsebox*, .silky-layout-content) matched NOTHING, which
// is why the empty line never appeared. Kept in sync with jmvtabreg.js's injectTabxCss.
var injectTabxCss = function() {
    if (document.getElementById("tabx-css")) return;
    var s = document.createElement("style");
    s.id = "tabx-css";
    s.textContent =
        // folder box (width: largest): collapse the ~200px floor but keep a comfortable minimum so it
        // stays the wider of the two boxes. Tune the 260px to taste -- this is the "folder a bit wider" knob.
        "input.silky-option-largest-text{min-width:260px !important;width:100% !important;box-sizing:border-box;}" +
        // file-name box (width: large): collapse its floor fully so it takes only its stretch share.
        "input.silky-option-large-text{min-width:0 !important;width:100% !important;box-sizing:border-box;}" +
        ".jmv-collapse-view:not(.view-colapsed){padding-bottom:10px;}";
    document.head.appendChild(s);
};

// Phase o: draw the rule above the (out-of-hierarchy) Export block via a border-top on its `margin:
// large` container, replacing a former <hr> Label jamovi rendered as raw escaped text.
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

// Push a row-1/row-2 control to the BOTTOM of its (taller) row. jamovi renders each control as a grid
// item in its row; walking up to that row-item cell and setting `align-self: flex-end` drops the control
// to the bottom of the row track (which the tall Export button / the text boxes define). The row-item
// cell is the one whose grid is a direct child of the export block's `margin: large` container -- that
// test finds it regardless of any inner label wrapper. Re-applied each onUpdate (jamovi re-renders drop
// inline styles, the same reason the width fix is a persistent stylesheet).
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
    bottomAlignInRow(ui, "export_format");   // Format combo -> bottom of row 1 (aligns with Export button)
    bottomAlignInRow(ui, "xl_replace");      // Replace checkbox -> bottom of row 1
    bottomAlignInRow(ui, "extCtrl");         // ".ext" text -> bottom of the path row
    renderRefPicker(ui);   // defined below (call-time resolution)
};

// ---- Phase 7g-ii / 20g-ii: the level control (levelsCtrl) --------------------------------
// It does BOTH things a user does to a variable's levels -- reorder them and merge them -- because
// they are one object: a merge is a run of CONSECUTIVE levels in the order the user chose, so a
// separate widget would have had to mirror this one's order. Each axis gets one FULL-WIDTH row.
// A 2-level collapsible tree, grouped by axis:
//   L1 axis (Row / Column / Table variables, open, left-indented)  >  L2 "<var> : N levels - reorder"
//   (collapsed; ONE click opens the level list). Each <details> has a Material grey tint + border.
// An open L2 shows a jamovi-styled selectable level list (click a level to SELECT it -- first selected by
// default, highlighted in jamovi's list-selection blue #b5caef) plus an Up/Down button pair BELOW the list
// acting on the selected level; the Up/Down ARROW KEYS do the same when the list is focused. The order is
// stored back to the `levels_order` Array option (one {var, levels} per reordered var); R reads it via
// jmvtab_levels_order(). `levels_order` is `hidden: true` in .a.yaml so the compiler does NOT auto-generate
// a default control for it (this control is the only UI). Levels are read as LevelSelector does:
// requestData('column', {properties:['measureType','levels']}). Numeric col_vars show a "no levels" note.

var TABX = {
    axis:    "margin:6px 6px 6px 12px;border:1px solid rgba(0,0,0,0.16);border-radius:4px;background:rgba(0,0,0,0.06);",  // left-indented from the outline
    axisTitle: "font-weight:600;padding:5px 8px;",   // non-collapsible axis header (no caret / pointer)
    varD:    "margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    varSum:  "display:block;list-style:none;padding:4px 8px;cursor:pointer;",
    note:    "padding:4px 8px;opacity:0.65;font-style:italic;",
    hint:    "padding:8px;opacity:0.65;font-style:italic;",
    // ref picker: one Material line per variable = a FIXED-width bold name column + a <select>
    // drop-down (current ref). Fixed name column -> all drop-downs align and share ONE width; the
    // whole row is ~2/3 wide so the drop-down has room (name width no longer drives it).
    refRow:  "display:grid;grid-template-columns:120px 1fr;align-items:center;gap:8px;width:66%;min-width:300px;box-sizing:border-box;padding:5px 8px;margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    refName: "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    refSel:  "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    refNote: "opacity:0.6;font-style:italic;",
    // a NUMERIC variable's row in the level box: name, ": numeric", and its `shape` drop-down. Same
    // tint and border as a factor's <details>, so the two read as one list.
    numRow:  "display:grid;grid-template-columns:max-content max-content 1fr;align-items:center;gap:8px;margin:4px 6px;padding:4px 8px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    shapeSel:"width:100%;max-width:220px;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    refHint: "padding:6px 8px 2px 8px;opacity:0.7;font-style:italic;"
};

// State persisting across rebuilds. levelsCache makes renderTree() SYNCHRONOUS after the first fetch --
// a deferred async swap was racing the user's in-place edits (the "2nd click does nothing, then all
// changes appear later" bug). lastVarSig + a root marker let the frequent `updated` event skip the
// rebuild during in-place reorder moves.
var openState = {};       // "<axis|var>:<key>" -> open bool
var levelsCache = {};     // var -> [labels] natural order | null (numeric/no-levels) | FETCHING sentinel
var mtypeCache = {};      // var -> jamovi measureType ("nominal" | "ordinal" | "continuous" | "id")
var FETCHING = {};
var lastVarSig = null;    // reorder-tree variable signature
var lastRefSig = null;    // ref-picker signature (vars + pct + color + shape + levels + OR)

// A shared level-fetch completed (either control): re-render BOTH the reorder tree and the ref
// picker, since they share `levelsCache` -- so a var whose levels one control fetched is not left
// on a "..." placeholder in the other. Both renders are idempotent given the cache.
var afterFetch = function(ui) {
    if (ui.levelsCtrl && ui.levelsCtrl.$el) renderTree(ui);
    if (ui.refPickerCtrl  && ui.refPickerCtrl.$el)  renderRefPicker(ui);
};

// THE column fetch, written once: the three renderers that need a column's levels all asked for the
// same two properties with the same guard and the same catch. `measureType` is kept beside the
// labels because an ORDERED factor may be merged but not reordered, and a CONTINUOUS one is offered
// a `shape` instead of a level list -- both are facts about the column, not about the caller.
var fetchLevels = function(ui, ctrlName, v) {
    if (!v || (v in levelsCache) || !ui[ctrlName]) return;
    levelsCache[v] = FETCHING;                       // guard against duplicate in-flight fetches
    ui[ctrlName].requestData("column", { columnName: v, properties: ["measureType", "levels"] })
        .then(function(col) {
            mtypeCache[v]  = col ? col.measureType : "continuous";
            levelsCache[v] = (!col || col.measureType === "continuous")
                ? null : col.levels.map(function(l) { return l.label; });
            afterFetch(ui);
        })
        .catch(function() {
            mtypeCache[v] = "continuous"; levelsCache[v] = null; afterFetch(ui);
        });
};
// The cached labels, or `undefined` while the fetch is in flight (which is what draws a placeholder).
var cachedLevels = function(v) {
    var c = (v in levelsCache) ? levelsCache[v] : undefined;
    return (c === FETCHING) ? undefined : c;
};

// Phase 15c: a NON-collapsible titled box for the AXIS level of the reorder tree ("Row variables" /
// "Column variables" / "Table variables") -- same Material tint/border as the old <details>, but a
// plain title <div> (no <summary>, caret or toggle), always open. The PER-VARIABLE nodes (makeVarNode)
// stay collapsible <details> -- each level already has its own collapse box.
var makeTitledBox = function(boxStyle, titleStyle, titleText) {
    var d = document.createElement("div");
    d.style.cssText = boxStyle;
    var t = document.createElement("div");
    t.style.cssText = titleStyle;
    t.textContent = titleText;
    d.appendChild(t);
    return d;
};

// ⚠ Each helper below GUARDS on its option existing. A hidden, control-less option only
// resolves through `ui.<name>` once the generated .h.R declares it, and that file LAGS a
// .a.yaml edit until the maintainer's next prepare() -- an unguarded read there throws and
// takes the whole `update` handler with it, so the panel goes inert rather than degrading.
// Drop levels_order entries whose variable is no longer selected (guarded setValue -> no loop).
var reconcileLevelOrder = function(ui, selected) {
    if (!ui.levels_order) return;
    var cur = utils.clone(ui.levels_order.value(), []);
    var kept = [];
    for (var i = 0; i < cur.length; i++)
        if (selected.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.levels_order.setValue(kept);
};

// The order to display for `v`: the stored entry (kept for still-present levels, new levels
// appended) if any, else the column's natural level order.
var storedOrder = function(ui, v, natural) {
    if (!ui.levels_order) return natural;
    var arr = utils.clone(ui.levels_order.value(), []);
    for (var i = 0; i < arr.length; i++) {
        if (arr[i].var === v && arr[i].levels && arr[i].levels.length) {
            var out = [];
            arr[i].levels.forEach(function(l) {
                if (natural.indexOf(l) >= 0 && out.indexOf(l) < 0) out.push(l);
            });
            natural.forEach(function(l) { if (out.indexOf(l) < 0) out.push(l); });
            return out;
        }
    }
    return natural;
};

// --- the per-numeric-variable `shape` (the level box's numeric rows) --------------------------
// Same get / write / reconcile idiom as levels_order above. ⚠ The DEFAULT is stored as NO entry:
// on an index axis that default is `"auto"`, which `shape =` does not accept as a value at all (it
// is the absence of one), and on a column axis it is `"linear"`. One rule, `defaultShape` being
// whichever the axis leads with.
var shapeSelected = function(ui, v) {
    if (!ui.shape) return "";
    var arr = utils.clone(ui.shape.value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i].shape == null ? "" : String(arr[i].shape));
    return "";
};
var writeShape = function(ui, v, sval, defaultShape) {
    if (!ui.shape) return;
    var arr = utils.clone(ui.shape.value(), []), kept = [];
    for (var i = 0; i < arr.length; i++) if (arr[i].var !== v) kept.push(arr[i]);
    if (sval && sval !== defaultShape) kept.push({ var: v, shape: String(sval) });
    ui.shape.setValue(kept);
};
var reconcileShapes = function(ui, selected) {
    if (!ui.shape) return;
    var cur = utils.clone(ui.shape.value(), []), kept = [];
    for (var i = 0; i < cur.length; i++)
        if (selected.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.shape.setValue(kept);
};

// Write the full ordered levels of `v` back to the levels_order option (create/replace its entry). Store a
// COPY of `lv` -- never the caller's live working array, else later in-place swaps would alias the option
// value and setValue() could miss the change.
var writeOrder = function(ui, v, lv) {
    if (!ui.levels_order) return;
    var copy = lv.slice();
    var arr = utils.clone(ui.levels_order.value(), []);
    var found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, levels: copy }; found = true; break; }
    if (!found) arr.push({ var: v, levels: copy });
    ui.levels_order.setValue(arr);
};

// --- BEGIN SHARED (dev/generate_jamovi_js.R: copied from jamovi/js/jmvtab.js) -- do not edit ---
// Phase 20g-ii: THE level list, with the merge tick-boxes -- the ONE widget both analyses show.
// jmvtab hosts it inside the level-order control (reorder + merge); jmvtabreg hosts it off each
// factor predictor's reference row (merge only -- `ref` picks the baseline explicitly, so there is
// nothing an order would mean there). It is copied verbatim into jmvtabreg.js by the generator, and
// `check` mode fails on drift -- so it is SELF-CONTAINED: its own styles, its own state, and no
// reference to either file's TABX object.
//
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
    body:  "padding:2px 8px 8px 8px;width:100%;box-sizing:border-box;",
    // 3 columns: level | merge tick | merged name. The name cell spans its run with grid-row/span,
    // which is why this is a grid and not the <ul> it replaced.
    // ⚠ THE TWO RIGHT COLUMNS ARE FIXED IN PIXELS, AND THE BOX IS width:100%. Everything else made
    // the widget resize under the pointer: `auto` sized column 2 to the tick-box (so it changed the
    // moment one appeared), `minmax(96px,1fr)` grew column 3 with whatever was typed in it, and the
    // overflow scrollbar took width away on expand -- `scrollbar-gutter:stable` reserves it always.
    grid:  "display:grid;grid-template-columns:minmax(0,1fr) 72px 200px;align-items:stretch;margin:4px 0;border:1px solid rgba(0,0,0,0.25);border-radius:3px;background:#fff;color:#000;max-height:220px;overflow-y:auto;scrollbar-gutter:stable;outline:none;width:100%;box-sizing:border-box;",
    head:  "padding:2px 8px;font-size:0.9em;color:#000;background:rgba(0,0,0,0.04);border-bottom:1px solid rgba(0,0,0,0.12);white-space:nowrap;",
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
var tabxmGroups = function(ui, v) {
    if (!ui.levels_collapse) return [];
    var arr = utils.clone(ui.levels_collapse.value(), []), out = [];
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v && arr[i].levels && arr[i].levels.length > 1)
            out.push({ label: arr[i].label || "", levels: arr[i].levels.slice() });
    return out;
};

// Replace ALL of `v`'s groups (other variables' entries untouched). Store a COPY of every level
// array -- setValue may keep it by reference, and a later in-place edit would then alias the option.
var tabxmWrite = function(ui, v, groups) {
    if (!ui.levels_collapse) return;
    var arr = utils.clone(ui.levels_collapse.value(), []), kept = [];
    for (var i = 0; i < arr.length; i++) if (arr[i].var !== v) kept.push(arr[i]);
    groups.forEach(function(g) {
        if (g.levels.length > 1)
            kept.push({ var: v, label: g.label || "", levels: g.levels.slice() });
    });
    ui.levels_collapse.setValue(kept);
};

// Drop entries whose variable is no longer selected (guarded setValue -> no loop).
var tabxmReconcile = function(ui, selected) {
    if (!ui.levels_collapse) return;
    var cur = utils.clone(ui.levels_collapse.value(), []), kept = [];
    for (var i = 0; i < cur.length; i++)
        if (selected.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.levels_collapse.setValue(kept);
};

// --- groups <-> ticks, and the post-merge display order ------------------------------------
// A tick is per LEVEL: true when this level is in the same group as the one before it in `order`.
var tabxmTicks = function(order, groups) {
    var runOf = {};
    groups.forEach(function(g, k) { g.levels.forEach(function(l) { runOf[l] = k; }); });
    return order.map(function(l, i) {
        return i > 0 && runOf[l] !== undefined && runOf[l] === runOf[order[i - 1]];
    });
};

// Rebuild the groups from the ticks, carrying each run's typed label over: an exact level-set match
// first, else the label of the group that held the run's first level (so extending a run keeps it).
var tabxmFromTicks = function(order, ticks, old) {
    var runs = [], cur = null;
    order.forEach(function(l, i) {
        if (i > 0 && ticks[i]) cur.push(l); else { cur = [l]; runs.push(cur); }
    });
    var labelFor = function(levels) {
        var key = levels.slice().sort().join(""), byFirst = "";
        for (var k = 0; k < old.length; k++) {
            if (old[k].levels.slice().sort().join("") === key) return old[k].label || "";
            if (!byFirst && old[k].levels.indexOf(levels[0]) >= 0) byFirst = old[k].label || "";
        }
        return byFirst;
    };
    return runs.filter(function(r) { return r.length > 1; })
               .map(function(r) { return { label: labelFor(r), levels: r }; });
};

// The levels the TABLE will show, in display order: each run replaced by its merged label (its
// joined levels when the box is empty -- the same default R applies). Used by the reference pickers,
// whose choices must be levels that still exist after the merge.
var tabxmDisplayOrder = function(order, groups) {
    var labOf = {};
    groups.forEach(function(g) {
        var lab = g.label || g.levels.join(", ");
        g.levels.forEach(function(l) { labOf[l] = lab; });
    });
    var out = [];
    order.forEach(function(l) {
        var lab = (labOf[l] !== undefined) ? labOf[l] : l;
        if (out.indexOf(lab) < 0) out.push(lab);
    });
    return out;
};

// --- the widget ----------------------------------------------------------------------------
// `onOrder(newOrder)` is supplied by a host that also offers reordering (jmvtab) and is null
// otherwise (jmvtabreg, whose producer has no `levels_order` argument to write to); the ▲/▼ bar and
// the arrow keys appear only when it is given.
// `canOrder = false` keeps the bar but GREYS it: an ORDERED factor already has the order its levels
// mean, so moving one is meaningless -- while merging two contiguous ordinal levels is not, and its
// tick-boxes stay live.
var tabxmBuildList = function(ui, v, initialOrder, onOrder, canOrder) {
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

    var selected = function() {
        var s = tabxmSel[v];
        return (s && order.indexOf(s) >= 0) ? s : order[0];
    };
    var paint = function() {
        var sel = selected();
        Array.prototype.forEach.call(grid.querySelectorAll("[data-lab]"), function(el) {
            el.style.background = (el.getAttribute("data-lab") === sel) ? TABXM_SEL : "";
        });
    };
    var commit = function() {
        ticks[0] = false;                                  // the first level can never merge upwards
        groups = tabxmFromTicks(order, ticks, groups);
        tabxmWrite(ui, v, groups);
    };
    var cell = function(style, col, row, span) {
        var d = document.createElement("div");
        d.style.cssText = style;
        d.style.gridColumn = String(col);
        d.style.gridRow = span ? (String(row) + " / span " + String(span)) : String(row);
        return d;
    };
    var renderRows = function() {
        grid.innerHTML = "";
        (canMerge ? ["level", "merge", "merged name"] : ["level"]).forEach(function(t, k) {
            var h = cell(TABXM.head, k + 1, 1);
            h.textContent = t;
            grid.appendChild(h);
        });
        order.forEach(function(lab, i) {
            var row = i + 2;
            var l = cell(TABXM.lab, 1, row);
            l.setAttribute("data-lab", lab);
            l.textContent = lab;
            l.addEventListener("click", function() { tabxmSel[v] = lab; paint(); grid.focus(); });
            grid.appendChild(l);
            if (!canMerge) return;

            var t = cell(TABXM.tick, 2, row);
            if (i > 0) {                      // the first level has nothing above to merge into
                var cb = document.createElement("input");
                cb.type = "checkbox"; cb.checked = !!ticks[i];
                cb.title = "merge into the level above";
                cb.addEventListener("change", function() {
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
            var len = j - i, c = cell(TABXM.cell, 3, i + 2, len);
            if (len > 1) {
                var levels = order.slice(i, j);
                var box = document.createElement("input");
                box.type = "text"; box.style.cssText = TABXM.input;
                box.value = (cur.filter(function(g) { return g.levels[0] === levels[0]; })[0]
                             || {}).label || "";
                box.placeholder = levels.join(", ");
                // ⚠ `box` and `levels` are BOTH passed into the closure. They are `var`s inside a
                // while loop, i.e. one function-scoped binding shared by every handler -- so a
                // handler reading them directly would edit the LAST run whichever box was typed in.
                // `change` (commit on blur / Enter), NEVER `input`: see the header.
                box.addEventListener("change", function(lv, b) {
                    return function() {
                        var val = b.value.trim();
                        groups = tabxmFromTicks(order, ticks, groups);
                        groups.forEach(function(g) { if (g.levels[0] === lv[0]) g.label = val; });
                        tabxmWrite(ui, v, groups);
                    };
                }(levels, box));
                c.appendChild(box);
            }
            grid.appendChild(c);
            i = j;
        }
        paint();
    };
    var move = function(dir) {
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
        commit();
        onOrder(order);
        renderRows();
    };
    if (onOrder && canOrder) {
        // ⚠ ignore the arrow keys while a tick-box or the name box has focus, or typing a merged
        // label would reorder the levels underneath it.
        grid.addEventListener("keydown", function(e) {
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
        var mk = function(sym, dir) {
            var b = document.createElement("button");
            b.type = "button"; b.textContent = sym;
            b.style.cssText = canOrder ? TABXM.btn : TABXM.btnOff;
            if (!canOrder) {
                b.disabled = true;
                b.title = "an ordered variable already has the order its levels mean";
                return b;
            }
            b.addEventListener("click", function(e) { e.preventDefault(); grid.focus(); move(dir); });
            return b;
        };
        bar.appendChild(mk("▲", -1));
        bar.appendChild(mk("▼",  1));
        wrap.appendChild(bar);
    }
    return wrap;
};
// --- END SHARED ---

// jmvtab's host: the list, with reordering on -- a move writes the RAW displayed order to
// `levels_order` exactly as before. R maps that raw order through the merge spec
// (jmv_order_after_collapse), which is why the list can go on showing the SOURCE levels: that is
// what a tick-box UI must show, or a merge could not be undone.
var buildVarBody = function(ui, v, initialOrder, canOrder) {
    return tabxmBuildList(ui, v, initialOrder, function(order) { writeOrder(ui, v, order); },
                          canOrder);
};

// A NUMBER has no levels to reorder or merge, so its row is not a collapsible at all: it asks the
// one question a number does raise -- how it becomes rows (or columns), i.e. `tab(shape =)`. The
// value list depends on the AXIS: an index variable can only be CUT, a column variable may also keep
// a number (log / sqrt), which is the same rule shape_refuse_numeric_index() enforces R-side. Both
// lists are generated from VAR_SHAPES (the block at the top of this file).
var makeNumericNode = function(ui, v, isCol) {
    var row = document.createElement("div");
    row.style.cssText = TABX.numRow;
    var name = document.createElement("b");
    name.textContent = v;
    row.appendChild(name);
    var note = document.createElement("span");
    note.style.cssText = TABX.note; note.textContent = ": numeric";
    row.appendChild(note);
    if (!ui.shape) return row;      // the generated .h.R lags a .a.yaml edit -- show no dead control
    var offered = isCol ? TABX_SHAPES_COL : TABX_SHAPES_INDEX;
    var cur = shapeSelected(ui, v);
    if (!cur || offered.indexOf(cur) < 0) cur = offered[0];
    var sel = document.createElement("select");
    sel.style.cssText = TABX.shapeSel;
    offered.forEach(function(sh) {
        var o = document.createElement("option");
        o.value = sh; o.textContent = sh;
        if (sh === cur) o.selected = true;
        sel.appendChild(o);
    });
    sel.addEventListener("change", function() { writeShape(ui, v, sel.value, offered[0]); });
    row.appendChild(sel);
    return row;
};

// Build ONE merged, collapsed-by-default variable node: summary "<var> : N levels - reorder" (BOLD var
// name) -> one click opens the level list. `natural` is the column's level labels (from levelsCache):
// undefined = still loading, null = numeric/no-levels, array = factor levels (-> a buildVarBody list).
var makeVarNode = function(ui, v, axisLabel, natural, isCol) {
    if (natural === null) return makeNumericNode(ui, v, isCol);
    var vKey = "var:" + axisLabel + ":" + v;
    var varD = document.createElement("details");
    varD.style.cssText = TABX.varD;
    varD.open = (vKey in openState) ? openState[vKey] : false;
    var sum = document.createElement("summary");
    sum.style.cssText = TABX.varSum;
    var caret = document.createElement("span");
    caret.style.cssText = "display:inline-block;width:1.1em;";
    caret.textContent = varD.open ? "▾" : "▸";
    var name = document.createElement("b");             // bold variable name for visibility
    name.textContent = v;
    var rest = document.createTextNode(
        natural === undefined ? " ..." :
                                (" : " + storedOrder(ui, v, natural).length +
                                 " levels – click to reorder / merge"));
    sum.appendChild(caret); sum.appendChild(name); sum.appendChild(rest);
    varD.appendChild(sum);
    varD.addEventListener("toggle", function() {
        openState[vKey] = varD.open;
        caret.textContent = varD.open ? "▾" : "▸";
    });
    // An ORDERED factor keeps its merge tick-boxes and loses its arrows: see tabxmBuildList.
    if (natural && natural.length)
        varD.appendChild(buildVarBody(ui, v, storedOrder(ui, v, natural),
                                      mtypeCache[v] !== "ordinal"));
    return varD;
};

// Render the whole tree SYNCHRONOUSLY into $el (no deferred swap). A var whose levels aren't cached yet
// gets a "..." placeholder + a one-shot requestData that caches the levels and calls renderTree() again
// (synchronous this time -- no async swap can clobber an in-place edit). The `data-tabx-tree` marker lets
// the `updated` handler tell a jamovi $el re-render from a plain option write. Reorder MOVES update their
// list in place (buildVarBody) and never come through here.
var renderTree = function(ui) {
    if (!ui.levelsCtrl || !ui.levels_order || !ui.row_vars) return;
    var rowV = utils.clone(ui.row_vars.value(), []);
    var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
    var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
    lastVarSig = JSON.stringify([rowV, colV, tabV]);
    var all = rowV.concat(colV).concat(tabV);
    reconcileLevelOrder(ui, all);
    tabxmReconcile(ui, all);
    reconcileShapes(ui, all);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-tree", "1");
    // Phase 20g-ii: ONE FULL-WIDTH ROW PER AXIS. It used to be a two-column grid (Row | Column, with
    // Table below Row), which halved the width available to each level list -- and a list that now
    // carries a merged-NAME text box beside every run needs the whole pane.
    // the 3rd slot is the AXIS rule a numeric variable's `shape` picker reads: only a COLUMN
    // variable may keep a number (see makeNumericNode).
    var axes = [["Row variables", rowV, false], ["Column variables", colV, true],
                ["Table variables", tabV, false]];
    axes.forEach(function(ax) {
        var label = ax[0], vars = ax[1];
        if (vars.length === 0) return;
        var axD = makeTitledBox(TABX.axis, TABX.axisTitle, label);
        frag.appendChild(axD);
        vars.forEach(function(v) {
            axD.appendChild(makeVarNode(ui, v, label, cachedLevels(v), ax[2]));
            fetchLevels(ui, "levelsCtrl", v);
        });
    });
    if (all.length === 0) {
        var hint = document.createElement("div");
        hint.style.cssText = TABX.hint;
        hint.textContent = "Select row, column or table variables to reorder or merge their levels.";
        frag.appendChild(hint);
    }
    var root = ui.levelsCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

// ---- Phase 7g-iii: reference-level picker CustomControl (refPickerCtrl) --------------------
// One Material card per axis variable (row_vars under pct="row"/means, col_vars under pct="col"),
// each a SINGLE-SELECT list "[Total, ...levels in the reordered order...]" (radio dots; the selected
// one highlighted #b5caef). Stored by LABEL in the `ref_levels` option, so a level reorder keeps the
// reference and just re-orders the list. A ref2 section (the odds-ratio 2nd reference) is shown only
// when OR is active. Distinct from the reorder tree: flat cards + radio dots, no Up/Down buttons, no
// collapsible tree. Shares levelsCache / requestData / storedOrder with the reorder tree.

// Signature that triggers a rebuild (vars + pct + color + OR + levels_order). NOT ref_levels / ref2 -- a
// pick is an in-place repaint, so the user's own click never rebuilds (mirrors the reorder tree).
var refSig = function(ui) {
    var rowV = utils.clone(ui.row_vars.value(), []);
    var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
    var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
    var pct    = ui.pct   ? ui.pct.value()   : "no";
    var colorV = ui.color ? ui.color.value() : "no";
    var dispV  = ui.display ? ui.display.value() : "auto";
    var lo     = ui.levels_order ? utils.clone(ui.levels_order.value(), []) : [];
    // 20g-ii: a MERGE changes which levels exist, so the drop-downs must be rebuilt on it too --
    // more sharply than on a reorder, which only re-sorts them.
    var lc     = ui.levels_collapse ? utils.clone(ui.levels_collapse.value(), []) : [];
    // 22g-iii: a CUT decides whether a numeric variable has a reference row at all, so it belongs
    // here for the same reason a merge does.
    var sh     = ui.shape ? utils.clone(ui.shape.value(), []) : [];
    return JSON.stringify([rowV, colV, tabV, pct, colorV, dispV, lo, lc, sh]);
};

// Is an ODDS RATIO the comparison this table makes? That is what switches the reference picker to a
// first-level default and shows its ref2 section. Phase 19k: the retired `OR` option is gone -- the
// comparison is named by the COLOUR measure or by the DISPLAY, exactly as tab()'s own resolver reads
// it (the 19d chain), and both halves come from the generated tables above.
var orIsActive = function(ui) {
    var colorV = ui.color   ? ui.color.value()   : "no";
    var dispV  = ui.display ? String(ui.display.value() || "auto") : "auto";
    if (colorV === TABX_MEASURE_ODDS_RATIO) return true;
    for (var i = 0; i < TABX_DISPLAY_ODDS_RATIO_FIELDS.length; i++) {
        var f = TABX_DISPLAY_ODDS_RATIO_FIELDS[i];
        if (dispV === f || dispV.indexOf("{" + f + "}") === 0) return true;   // the PRIMARY token
    }
    return false;
};

// The stored reference for variable `v` in ref_levels ("" if the user has not picked one).
var refSelected = function(ui, v) {
    if (!ui.ref_levels) return "";
    var arr = utils.clone(ui.ref_levels.value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i].ref == null ? "" : String(arr[i].ref));
    return "";
};

// Set/replace variable `v`'s reference entry in ref_levels.
var writeRef = function(ui, v, refval) {
    if (!ui.ref_levels) return;
    var arr = utils.clone(ui.ref_levels.value(), []);
    var found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, ref: refval }; found = true; break; }
    if (!found) arr.push({ var: v, ref: refval });
    ui.ref_levels.setValue(arr);
};

// Drop ref_levels entries whose var is not in the active axis (guarded setValue -> no loop): clears
// stale entries after a pct row<->col switch or a removed variable.
var reconcileRefLevels = function(ui, activeVars) {
    if (!ui.ref_levels) return;
    var cur = utils.clone(ui.ref_levels.value(), []);
    var kept = [];
    for (var i = 0; i < cur.length; i++)
        if (activeVars.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.ref_levels.setValue(kept);
};

var choicesHasRef = function(choices, ref) {
    return !!ref && choices.some(function(c) { return c.ref === ref; });
};

// A compact single line: a BOLD variable/label name + a native <select> drop-down showing the current
// reference level (click it to pick another from the list). choices = [{ref, label}]; selectedRef is
// the value the drop-down opens on; onPick(ref) writes the chosen reference.
var refLineControl = function(nameText, choices, selectedRef, onPick) {
    var row = document.createElement("div"); row.style.cssText = TABX.refRow;
    var lab = document.createElement("b"); lab.style.cssText = TABX.refName; lab.textContent = nameText;
    row.appendChild(lab);
    var sel = document.createElement("select"); sel.style.cssText = TABX.refSel;
    choices.forEach(function(c) {
        var o = document.createElement("option");
        o.value = c.ref; o.textContent = c.label;
        if (c.ref === selectedRef) o.selected = true;
        sel.appendChild(o);
    });
    sel.addEventListener("change", function() { onPick(sel.value); });
    row.appendChild(sel);
    return row;
};

// Render one axis variable as a single line "<var> [ current ref level v ]" (fetching its levels if
// needed, like renderTree).
var renderRefVarCard = function(ui, frag, v, orActive) {
    var cached = cachedLevels(v);
    fetchLevels(ui, "refPickerCtrl", v);
    if (cached === undefined) {
        var ph = document.createElement("div"); ph.style.cssText = TABX.refRow;
        var b0 = document.createElement("b"); b0.style.cssText = TABX.refName; b0.textContent = v;
        var d0 = document.createElement("span"); d0.style.cssText = TABX.refNote; d0.textContent = "…";
        ph.appendChild(b0); ph.appendChild(d0);
        frag.appendChild(ph);
        return;
    }
    if (cached === null) {
        // A NUMBER left as a number has no reference to choose -- `tab()` offers none, so the row
        // that used to say "compared with its total" named a choice that did not exist. CUT into
        // groups it does have one; but the group LABELS are computed R-side from the data's own
        // quantiles, so the only references nameable here are the positional ones `ref =` accepts.
        var sh = shapeSelected(ui, v);
        if (!sh || sh === "auto" || sh === "linear" || sh === "log" || sh === "sqrt") return;
        var numChoices = [{ ref: "tot", label: "Total" }, { ref: "first", label: "First group" },
                          { ref: "last", label: "Last group" }];
        var st0 = refSelected(ui, v);
        frag.appendChild(refLineControl(
            v, numChoices, choicesHasRef(numChoices, st0) ? st0 : "tot",
            function(r) { writeRef(ui, v, r); }));
        return;
    }
    // 20g-ii: the choices are the levels the TABLE will show -- a merged run is ONE level, under its
    // merged name -- because a reference naming a level the merge dissolved does not exist any more.
    var levels = tabxmDisplayOrder(storedOrder(ui, v, cached), tabxmGroups(ui, v));
    var choices = [{ ref: "tot", label: "Total" }].concat(
        levels.map(function(l) { return { ref: l, label: l }; }));
    var effDefault = orActive ? levels[0] : "tot";   // ref="auto" -> "first" under OR, else "tot"
    var stored = refSelected(ui, v);
    var selRef = choicesHasRef(choices, stored) ? stored : effDefault;
    frag.appendChild(refLineControl(v, choices, selRef, function(r) { writeRef(ui, v, r); }));
};

// Render the ref2 (odds-ratio 2nd reference) section: one GLOBAL drop-down over the OTHER axis's
// levels + First/Total, with a one-line explanation. Shown only when OR is active.
var renderRef2Section = function(ui, frag, pct, ref2var) {
    var levels = ref2var ? cachedLevels(ref2var) : undefined;
    fetchLevels(ui, "refPickerCtrl", ref2var);
    var lvlChoices = (levels && levels.length)
        ? tabxmDisplayOrder(storedOrder(ui, ref2var, levels), tabxmGroups(ui, ref2var))
              .map(function(l) { return { ref: l, label: l }; })
        : [];
    var choices = [{ ref: "first", label: "First" }, { ref: "tot", label: "Total" }].concat(lvlChoices);
    var where = (pct === "col") ? "row" : "column";
    var note = document.createElement("div"); note.style.cssText = TABX.refHint;
    note.textContent = "Odds ratios — 2nd reference (the " + where + " each odds ratio is compared to):";
    frag.appendChild(note);
    var selRef = ui.ref2 ? ui.ref2.value() : "first";
    if (!choicesHasRef(choices, selRef)) selRef = "first";
    frag.appendChild(refLineControl("reference " + where, choices, selRef,
        function(r) { if (ui.ref2) ui.ref2.setValue(r); }));
};

// Render the whole ref picker SYNCHRONOUSLY into $el (mirrors renderTree). ref_levels/ref2 picks are
// in-place repaints and never come through here.
var renderRefPicker = function(ui) {
    if (!ui.refPickerCtrl || !ui.ref_levels || !ui.row_vars) return;
    lastRefSig = refSig(ui);
    var pct  = ui.pct ? ui.pct.value() : "no";
    var rowV = utils.clone(ui.row_vars.value(), []);
    var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
    var orActive = orIsActive(ui);
    var axisVars = (pct === "col") ? colV : rowV;
    reconcileRefLevels(ui, axisVars);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-refpick", "1");

    if (axisVars.length === 0) {
        var hint = document.createElement("div"); hint.style.cssText = TABX.hint;
        hint.textContent = (pct === "col")
            ? "Select column variables to choose their reference column."
            : "Select row variables to choose their reference row.";
        frag.appendChild(hint);
    } else {
        axisVars.forEach(function(v) { renderRefVarCard(ui, frag, v, orActive); });
    }

    if (orActive) {
        var ref2axis = (pct === "col") ? rowV : colV;   // OR's 2nd reference is on the OTHER axis
        renderRef2Section(ui, frag, pct, ref2axis[0]);
    }

    var root = ui.refPickerCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

module.exports = {

    // Root view update. Bound explicitly via `events: update:` in .u.yaml; `view_updated` is the
    // jus-3.0 naming-convention alias -- keep both so initial sync fires whichever the build uses
    // (both are idempotent). Fixes the panel-hang from the jus-2.0 `context.clone` pattern.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box (row/col/tab) changed: re-render the reference picker AND the level-reorder
    // control. Shared by all three VariablesListBoxes (see .u.yaml `change` events).
    onChange_vars: function(ui) {
        applyVarEnables(ui);
        renderRefPicker(ui);
        renderTree(ui);
    },

    // pct / OR / color changed: re-render the reference picker so its axis (row vs col), effective
    // default and the ref2 (odds-ratio) section follow immediately. A bare CustomControl does not get
    // a reliable `updated` for OTHER options' changes, so these radios are wired explicitly (.u.yaml).
    onChange_refopts: function(ui) {
        renderRefPicker(ui);
    },

    // levelsCtrl: build on create. On `updated`, re-render ONLY when the variable set changed OR
    // jamovi replaced our $el subtree (marker gone) -- a reorder MOVE fires `updated` (via setValue) with
    // the same vars + marker present, so it is SKIPPED and the in-place list update stands. This is what
    // fixes the "2nd click does nothing" bug (the old rebuild-and-swap clobbered the in-place edit).
    levelsCtrl_creating: function(ui) { renderTree(ui); },
    levelsCtrl_updated:  function(ui) {
        if (!ui.levelsCtrl || !ui.row_vars) return;
        var rowV = utils.clone(ui.row_vars.value(), []);
        var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
        var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
        var sig = JSON.stringify([rowV, colV, tabV]);
        var root = ui.levelsCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-tree") === "1");
        if (sig === lastVarSig && present) return;
        renderTree(ui);
    },

    // refPickerCtrl: build on create. On `updated`, re-render ONLY when the signature (vars / pct /
    // color / OR / levels_order) changed OR jamovi replaced our $el subtree (marker gone). A reference
    // PICK writes ref_levels/ref2 -- not in the signature -- so it is SKIPPED and the in-place repaint
    // stands; a level reorder IS in the signature, so the lists re-order while the by-label selection
    // is preserved.
    refPickerCtrl_creating: function(ui) { renderRefPicker(ui); },
    refPickerCtrl_updated:  function(ui) {
        if (!ui.refPickerCtrl || !ui.row_vars) return;
        var sig = refSig(ui);
        var root = ui.refPickerCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-refpick") === "1");
        if (sig === lastRefSig && present) return;
        renderRefPicker(ui);
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

    // Reset the export action button shortly after it is clicked, so a second export re-fires the
    // change event.
    exportExcel_changed: function(ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function() {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    },

    // Reset the export folder + file name to their defaults, then clear the action so it can re-fire.
    resetPath_changed: function(ui) {
        if (ui.resetPath && ui.resetPath.value()) {
            if (ui.export_dir)      ui.export_dir.setValue("~/Documents");
            if (ui.export_filename) ui.export_filename.setValue("Table");
            ui.resetPath.setValue(false);
        }
    }

};
