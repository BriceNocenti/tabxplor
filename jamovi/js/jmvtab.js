// Custom UI events for the jmvtab (Crosstables) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep
// it lean. See dev/tabxplor_2.0.0_jamovi_dev.md (Phase 22g-iv: the per-variable table; §14 export).
// Everything about a VARIABLE -- its level order, its merges, how a number is cut, what it is
// compared to -- is one table, built by the SHARED block and described by VAR_TABLE_HOST below.
// jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

// --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---
// Generated from R/fmt_class.R (MEASURES), R/tab-display.R (DISPLAY_TOKENS) and
// R/var-shape.R (VAR_SHAPES). Re-run dev/generate_jamovi_js.R after changing them;
// the suite checks this block (test-jamovi-vocabulary.R).
var TABX_MEASURE_ODDS_RATIO = "odds_ratio";
var TABX_DISPLAY_ODDS_RATIO_FIELDS = ["or"];
var TABX_SHAPES_INDEX = ["auto", "sd_bands", "median", "terciles", "quartiles", "quintiles", "deciles", "values_to_levels"];
var TABX_SHAPES_COL = ["linear", "log", "sqrt", "sd_bands", "median", "terciles", "quartiles", "quintiles", "deciles", "values_to_levels"];
var TABX_SHAPES_CUT = ["sd_bands", "median", "terciles", "quartiles", "quintiles", "deciles", "values_to_levels"];
var TABX_SHAPE_LABEL = { "linear": "linear (numeric)", "log": "log (numeric)", "sqrt": "sqrt (numeric)", "sd_bands": "sd_bands (cut)", "median": "median (cut)", "terciles": "terciles (cut)", "quartiles": "quartiles (cut)", "quintiles": "quintiles (cut)", "deciles": "deciles (cut)", "values_to_levels": "values_to_levels" };
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
    // ...and the design effect is a statement ABOUT the weights: with none there is nothing to say.
    var w = ui.wt ? ui.wt.value() : null;
    if (ui.design_effect) ui.design_effect.setEnabled(!!(w && w.length > 0));
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
    renderVarTable(ui);    // the per-variable table (defined below: call-time resolution)
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
        sel.title = "how this number becomes groups (shape =)";
        c.levels.appendChild(sel); return;
    }
    var natural = kind.cached || [];
    if (natural.length === 0) return;
    // the COUNT is the original one -- a merge is a statement about those levels, not a new set of
    // them -- and the instruction beside it says which way the click goes.
    var b = document.createElement("span"); b.style.cssText = TABXV.lvlBtn + TABXV.dotted;
    var n = document.createElement("span"); n.textContent = String(natural.length) + " levels";
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
                hd.innerHTML = (k === 0) ? (g.label || col.head) : col.head;   // our own strings only
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
            if (i === 0) t.textContent = "merge";   // the column names itself in its empty cell
            if (i > 0) {                      // the first level has nothing above to merge into
                var cb = document.createElement("input");
                cb.type = "checkbox"; cb.checked = !!ticks[i];
                cb.title = "merge into the level above";
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
                b.title = "an ordered variable already has the order its levels mean";
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

// ---- The crosstab host: what the shared table lists, and what its reference cell is ----------
// Three axis groups (row / column / table variables), the ▲/▼ bar on, and a reference cell that
// follows `pct`: a reference is a choice about the axis the percentages are computed ON, so only
// that axis's variables have one. An OFF-AXIS variable's cell is EMPTY -- with one exception below.

// Is an ODDS RATIO the comparison this table makes? That is what switches the reference to a
// first-level default and puts the odds ratio's SECOND reference on the other axis. The comparison
// is named by the COLOUR measure or by the DISPLAY, exactly as tab()'s own resolver reads it, and
// both halves come from the generated tables at the top of this file.
var orIsActive = function (ui) {
    var colorV = ui.color   ? ui.color.value()   : "no";
    var dispV  = ui.display ? String(ui.display.value() || "auto") : "auto";
    if (colorV === TABX_MEASURE_ODDS_RATIO) return true;
    for (var i = 0; i < TABX_DISPLAY_ODDS_RATIO_FIELDS.length; i++) {
        var f = TABX_DISPLAY_ODDS_RATIO_FIELDS[i];
        if (dispV === f || dispV.indexOf("{" + f + "}") === 0) return true;   // the PRIMARY token
    }
    return false;
};

var tabVarsOf = function (ui, nm) { return ui[nm] ? utils.clone(ui[nm].value(), []) : []; };

var VAR_TABLE_HOST = {
    ctrl: "varTableCtrl",
    // THREE columns: `tab()` has no per-variable scaling, so a 4th would be dead width in a pane
    // that is already the narrowest thing jamovi shows. ⚠ the name column's minmax FLOOR: with
    // minmax(0,1fr) it collapsed to nothing behind the fixed columns and the names vanished.
    cols: [
        { key: "name",   head: "variable",         width: "minmax(90px,1fr)" },
        { key: "levels", head: "levels / shape =", width: "165px",
          tip: "a factor shows how many levels it has; a number chooses how it is cut into groups" },
        { key: "ref",    head: "ref = <i>(reference)</i>", width: "180px",
          tip: "what each cell is compared to" }
    ],
    emptyHint: "Select row, column or table variables to order, merge or cut their levels.",
    mergeTip:  "click to relevel",
    closeTip:  "click to close",
    orderOpt:  "levels_order",
    isCut:     function (sh) { return TABX_SHAPES_CUT.indexOf(sh) >= 0; },

    // the 3rd slot is the AXIS rule a number reads: only a COLUMN variable may stay a number (its
    // reading is its mean), which is the same rule shape_refuse_numeric_index() enforces R-side.
    groups: function (ui) {
        return [{ label: "Row variables",    vars: tabVarsOf(ui, "row_vars"), numericMayKeep: false },
                { label: "Column variables", vars: tabVarsOf(ui, "col_vars"), numericMayKeep: true  },
                { label: "Table variables",  vars: tabVarsOf(ui, "tab_vars"), numericMayKeep: false }];
    },

    // ⚠ ONLY what the table does not itself write (see the SHARED header). `pct` decides which axis
    // carries the reference, `color` / `display` whether an odds ratio is in force, and
    // `cleannames` how every level in it is spelt.
    sig: function (ui) {
        return JSON.stringify([tabVarsOf(ui, "row_vars"), tabVarsOf(ui, "col_vars"),
                               tabVarsOf(ui, "tab_vars"),
                               ui.pct ? ui.pct.value() : "no",
                               ui.color ? ui.color.value() : "no",
                               ui.display ? ui.display.value() : "auto",
                               ui.cleannames ? ui.cleannames.value() : true]);
    },

    shapes: function (g) { return g.numericMayKeep ? TABX_SHAPES_COL : TABX_SHAPES_INDEX; },

    // Drop ref_levels entries whose variable is not on the ACTIVE axis: that is what clears a stale
    // entry after a pct row<->col switch, and what stops a cross-axis reference reaching R.
    reconcile: function (ui) {
        reconcileArr(ui, "ref_levels", tabAxisVars(ui));
    },

    // A `shape` pick can change what a reference MEANS (a number left a number has none at all, a
    // cut one takes a positional reference), so a stored value from the other vocabulary goes.
    varSync: function (ui, v, kind) {
        if (!kind.isNumber) return;
        var stored = arrGet(ui, "ref_levels", v, "ref");
        if (!stored) return;
        if (!kind.isCut || ["tot", "first", "last"].indexOf(stored) < 0)
            arrWrite(ui, "ref_levels", v, "ref", "");
    },

    refCell: function (ui, cell, v, kind) {
        var pct = ui.pct ? ui.pct.value() : "no";
        if (tabAxisVars(ui).indexOf(v) < 0) return tabRef2Cell(ui, cell, v, pct);
        if (kind.loading) return;
        if (kind.isNumber && !kind.isCut) return;     // a number left a number has no reference
        var choices, labels, def;
        // ⚠ AN ODDS RATIO IS NEVER READ AGAINST A TOTAL -- a marginal percentage that includes the
        // cell itself is not a category -- so under one "tot" is not offered at all, not merely
        // demoted from the default. A value STORED before the switch is deliberately left alone:
        // tab()'s own leaf falls back to the first level silently (plain_resolve), so turning the
        // odds ratio off gives the user their own choice back.
        var or = orIsActive(ui);
        if (kind.isCut) {
            // A cut number DOES have groups, but their labels are computed R-side from the data's
            // own quantiles -- so the only references nameable here are the positional ones.
            choices = or ? ["first", "last"] : ["tot", "first", "last"];
            labels  = { tot: "Total", first: "First group", last: "Last group" };
            def     = or ? "first" : "tot";
        } else {
            // The choices are the levels the TABLE will show -- a merged run is ONE level, under its
            // merged name -- because a reference naming a level the merge dissolved does not exist.
            var levels = tabxvLevels(ui, VAR_TABLE_HOST, v, kind.cached);
            if (levels.length === 0) return;
            choices = or ? levels : ["tot"].concat(levels);
            labels  = function (o) { return (o === "tot") ? "Total" : tabxvClean(ui, o); };
            def     = or ? levels[0] : "tot";
        }
        var stored = arrGet(ui, "ref_levels", v, "ref");
        cell.appendChild(makeSelect(TABXV.sel, choices, labels,
                                    choices.indexOf(stored) >= 0 ? stored : def,
                                    function (r) { arrWrite(ui, "ref_levels", v, "ref", r); }));
    }
};

// The axis a reference is chosen on: col_vars under col%, row_vars otherwise (row% and means).
var tabAxisVars = function (ui) {
    var pct = ui.pct ? ui.pct.value() : "no";
    return tabVarsOf(ui, (pct === "col") ? "col_vars" : "row_vars");
};

// The odds ratio's SECOND reference. It is a statement about the OTHER axis, and it exists only
// while an odds ratio is in force -- so rather than a control of its own it borrows the reference
// cell of the first variable of that axis, and says what it is in a tooltip. Every other off-axis
// cell stays empty: a reference the table does not use must not be offered as though it did.
var tabRef2Cell = function (ui, cell, v, pct) {
    if (!ui.ref2 || !orIsActive(ui)) return;
    var other = tabVarsOf(ui, (pct === "col") ? "row_vars" : "col_vars");
    if (other.length === 0 || other[0] !== v) return;
    var lv = cachedLevels(v);
    var levels = (lv && lv.length) ? tabxvLevels(ui, VAR_TABLE_HOST, v, lv) : [];
    // no "tot": the second reference of an odds ratio is a CATEGORY, like the first.
    var choices = ["first"].concat(levels);
    var stored  = String(ui.ref2.value() || "first");
    var labs = function (o) { return (o === "first") ? "First" : tabxvClean(ui, o); };
    var sel = makeSelect(TABXV.sel, choices, labs,
                         choices.indexOf(stored) >= 0 ? stored : "first",
                         function (r) { ui.ref2.setValue(r); });
    sel.title = "odds ratios \u2013 the " + ((pct === "col") ? "row" : "column") +
                " each odds ratio is compared to (ref2 =)";
    // this ONE cell sits in the column headed `ref =` but writes the OTHER argument, so it says so.
    var tag = document.createElement("span");
    tag.style.cssText = TABXV.lvlHow + "white-space:nowrap;flex:0 0 auto;margin-right:4px;";
    tag.textContent = "ref2 =";
    cell.appendChild(tag);
    cell.appendChild(sel);
};

var renderVarTable = function (ui) { tabxvRender(ui, VAR_TABLE_HOST); };

module.exports = {

    // Root view update. Bound explicitly via `events: update:` in .u.yaml; `view_updated` is the
    // jus-3.0 naming-convention alias -- keep both so initial sync fires whichever the build uses
    // (both are idempotent). Fixes the panel-hang from the jus-2.0 `context.clone` pattern.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box (row/col/tab) changed: the per-variable table lists them, so it rebuilds.
    onChange_vars: function (ui) {
        applyVarEnables(ui);
        renderVarTable(ui);
    },

    // pct / color / display changed: the reference axis and the odds-ratio second reference follow
    // immediately. A bare CustomControl does not get a reliable `updated` for OTHER options'
    // changes, so these controls are wired explicitly (.u.yaml).
    onChange_refopts: function (ui) {
        renderVarTable(ui);
    },

    // varTableCtrl: build on create; on `updated`, rebuild only when the signature moved or jamovi
    // replaced our subtree (see the SHARED header's signature rule).
    varTableCtrl_creating: function (ui) { renderVarTable(ui); },
    varTableCtrl_updated:  function (ui) { tabxvUpdated(ui, VAR_TABLE_HOST); },

    // subtextCtrl: the full-width auto-grow <textarea> for the below-table note (Phase 15c).
    subtextCtrl_creating: function (ui) { renderSubtext(ui); },
    subtextCtrl_updated:  function (ui) { renderSubtext(ui); },

    // extCtrl: the small ".ext" label after the file name; follows the chosen format.
    extCtrl_creating: function (ui) { renderExt(ui); },
    extCtrl_updated:  function (ui) { renderExt(ui); },

    // The chosen format changed: update the file-extension label on the path line.
    export_format_changed: function (ui) {
        renderExt(ui);
    },

    // Reset the export action button shortly after it is clicked, so a second export re-fires the
    // change event.
    exportExcel_changed: function (ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function () {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    },

    // Reset the export folder + file name to their defaults, then clear the action so it can re-fire.
    resetPath_changed: function (ui) {
        if (ui.resetPath && ui.resetPath.value()) {
            if (ui.export_dir)      ui.export_dir.setValue("~/Documents");
            if (ui.export_filename) ui.export_filename.setValue("Table");
            ui.resetPath.setValue(false);
        }
    }

};
