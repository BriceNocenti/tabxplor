// Custom UI events for the jmvtab (Crosstables) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep
// it lean. See dev/tabxplor_1.4.0_jamovi_dev.md (§12 ref picker, §14 export) for the events API.
// jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

var exportLabels = { excel: "Excel", html: "HTML", md: "markdown" };

var setExportLabel = function(ui) {
    if (!ui.export_format || !ui.exportExcel) return;
    var fmt = ui.export_format.value();
    ui.exportExcel.setPropertyValue("label", "Export " + (exportLabels[fmt] || "Excel"));
};

// Pin the export button to the widest label's width ("Export markdown") so its TEXT changes with the
// format but its SIZE does not. Re-applied on each onUpdate (jamovi re-renders may drop inline styles).
var fixExportBtnWidth = function(ui) {
    var c = ui.exportExcel;
    if (!c || !c.$el || !c.$el[0]) return;
    var btn = c.$el[0].querySelector("button") || c.$el[0];
    btn.style.width = "150px";
    btn.style.boxSizing = "border-box";
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

// jamovi 2.6.44's TextBox `width:` enum caps at `largest` (200px) -- there is NO `auto` (the compiler
// rejects it). To let `subtext` and the export `path` fill their (stretchFactor) cell, clear the
// fixed-width `silky-option-<size>-text` cap in .js: widen the control root + every wrapper down to the
// input to width:100%. Re-applied on each onUpdate (jamovi may re-render the control and drop inline
// styles). Purely cosmetic.
var stretchTextBox = function(ui, name) {
    var c = ui[name];
    if (!c || !c.$el || !c.$el[0]) return;
    var root = c.$el[0];
    root.style.width = "100%"; root.style.maxWidth = "none";
    var inp = (c.$input && c.$input[0]) || root.querySelector("input");
    var node = inp, guard = 0;
    while (node && node !== root && guard++ < 6) {
        node.style.width = "100%"; node.style.maxWidth = "none";
        node = node.parentElement;
    }
    if (inp) inp.style.width = "100%";
};

var onUpdate = function(ui) {
    setExportLabel(ui);
    fixExportBtnWidth(ui);
    applyVarEnables(ui);
    renderSubtext(ui);
    stretchTextBox(ui, "export_dir");
    renderRefPicker(ui);   // defined below (call-time resolution)
};

// ---- Phase 7g-ii: level-reordering CustomControl (levelOrderCtrl) ------------------------
// A 2-level collapsible tree, grouped by axis:
//   L1 axis (Row / Column / Table variables, open, left-indented)  >  L2 "<var> : N levels - reorder"
//   (collapsed; ONE click opens the level list). Each <details> has a Material grey tint + border.
// An open L2 shows a jamovi-styled selectable level list (click a level to SELECT it -- first selected by
// default, highlighted in jamovi's list-selection blue #b5caef) plus an Up/Down button pair BELOW the list
// acting on the selected level; the Up/Down ARROW KEYS do the same when the list is focused. The order is
// stored back to the `levelOrder` Array option (one {var, levels} per reordered var); R reads it via
// jmvtab_levels_order(). `levelOrder` is `hidden: true` in .a.yaml so the compiler does NOT auto-generate
// a default control for it (this control is the only UI). Levels are read as LevelSelector does:
// requestData('column', {properties:['measureType','levels']}). Numeric col_vars show a "no levels" note.

var TABX_SEL = "#b5caef";   // jamovi's list-selection blue (.selected in analysisui.css)
var TABX = {
    axis:    "margin:6px 6px 6px 12px;border:1px solid rgba(0,0,0,0.16);border-radius:4px;background:rgba(0,0,0,0.06);",  // left-indented from the outline
    axisTitle: "font-weight:600;padding:5px 8px;",   // non-collapsible axis header (no caret / pointer)
    varD:    "margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    varSum:  "display:block;list-style:none;padding:4px 8px;cursor:pointer;",
    body:    "padding:2px 8px 8px 8px;",
    list:    "list-style:none;margin:4px 0;padding:0;border:1px solid rgba(0,0,0,0.25);border-radius:3px;background:#fff;color:#000;max-height:180px;overflow-y:auto;outline:none;",
    item:    "padding:2px 8px;cursor:pointer;white-space:nowrap;",
    bar:     "display:flex;gap:6px;",
    btn:     "width:30px;height:22px;line-height:1;padding:0;cursor:pointer;",
    note:    "padding:4px 8px;opacity:0.65;font-style:italic;",
    hint:    "padding:8px;opacity:0.65;font-style:italic;",
    // ref picker: one Material line per variable = a FIXED-width bold name column + a <select>
    // drop-down (current ref). Fixed name column -> all drop-downs align and share ONE width; the
    // whole row is ~2/3 wide so the drop-down has room (name width no longer drives it).
    refRow:  "display:grid;grid-template-columns:120px 1fr;align-items:center;gap:8px;width:66%;min-width:300px;box-sizing:border-box;padding:5px 8px;margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    refName: "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    refSel:  "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    refNote: "opacity:0.6;font-style:italic;",
    refHint: "padding:6px 8px 2px 8px;opacity:0.7;font-style:italic;"
};

// State persisting across rebuilds. levelsCache makes renderTree() SYNCHRONOUS after the first fetch --
// a deferred async swap was racing the user's in-place edits (the "2nd click does nothing, then all
// changes appear later" bug). lastVarSig + a root marker let the frequent `updated` event skip the
// rebuild during in-place reorder moves.
var openState = {};       // "<axis|var>:<key>" -> open bool
var levelsCache = {};     // var -> [labels] natural order | null (numeric/no-levels) | FETCHING sentinel
var FETCHING = {};
var lastVarSig = null;    // reorder-tree variable signature
var lastRefSig = null;    // ref-picker signature (vars + pct + color + OR + levelOrder)

// A shared level-fetch completed (either control): re-render BOTH the reorder tree and the ref
// picker, since they share `levelsCache` -- so a var whose levels one control fetched is not left
// on a "..." placeholder in the other. Both renders are idempotent given the cache.
var afterFetch = function(ui) {
    if (ui.levelOrderCtrl && ui.levelOrderCtrl.$el) renderTree(ui);
    if (ui.refPickerCtrl  && ui.refPickerCtrl.$el)  renderRefPicker(ui);
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

// Drop levelOrder entries whose variable is no longer selected (guarded setValue -> no loop).
var reconcileLevelOrder = function(ui, selected) {
    var cur = utils.clone(ui.levelOrder.value(), []);
    var kept = [];
    for (var i = 0; i < cur.length; i++)
        if (selected.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.levelOrder.setValue(kept);
};

// The order to display for `v`: the stored entry (kept for still-present levels, new levels
// appended) if any, else the column's natural level order.
var storedOrder = function(ui, v, natural) {
    var arr = utils.clone(ui.levelOrder.value(), []);
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

// Write the full ordered levels of `v` back to the levelOrder option (create/replace its entry). Store a
// COPY of `lv` -- never the caller's live working array, else later in-place swaps would alias the option
// value and setValue() could miss the change.
var writeOrder = function(ui, v, lv) {
    var copy = lv.slice();
    var arr = utils.clone(ui.levelOrder.value(), []);
    var found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, levels: copy }; found = true; break; }
    if (!found) arr.push({ var: v, levels: copy });
    ui.levelOrder.setValue(arr);
};

var selectedByVar = {};   // var -> selected level label (persists across rebuilds)

// Build a variable's reorder body: a jamovi-styled selectable level list + an Up/Down button pair BELOW.
// Click a level to select it (first is selected by default, highlighted #b5caef); Up/Down (buttons or,
// when the list is focused, the arrow keys) move the SELECTED level, which stays selected so it walks.
var buildVarBody = function(ui, v, initialOrder) {
    var wrap = document.createElement("div");
    wrap.style.cssText = TABX.body;
    var order = initialOrder.slice();

    var ul = document.createElement("ul");
    ul.style.cssText = TABX.list; ul.tabIndex = 0;

    var selected = function() {
        var s = selectedByVar[v];
        return (s && order.indexOf(s) >= 0) ? s : order[0];
    };
    var paint = function() {
        var sel = selected();
        Array.prototype.forEach.call(ul.children, function(li) {
            li.style.background = (li.getAttribute("data-lab") === sel) ? TABX_SEL : "";
        });
    };
    var renderRows = function() {
        ul.innerHTML = "";
        order.forEach(function(lab) {
            var li = document.createElement("li");
            li.style.cssText = TABX.item;
            li.setAttribute("data-lab", lab);
            li.textContent = lab;
            li.addEventListener("click", function() { selectedByVar[v] = lab; paint(); ul.focus(); });
            ul.appendChild(li);
        });
        paint();
    };
    var move = function(dir) {
        var sel = selected();
        var i = order.indexOf(sel), j = i + dir;
        if (j < 0 || j >= order.length) return;
        order[i] = order[j]; order[j] = sel;    // swap: the selected level moves to j
        selectedByVar[v] = sel;                 // selection follows it, so repeated moves walk it
        writeOrder(ui, v, order);
        renderRows();
    };
    ul.addEventListener("keydown", function(e) {
        if (e.key === "ArrowUp")        { e.preventDefault(); move(-1); }
        else if (e.key === "ArrowDown") { e.preventDefault(); move(1); }
    });
    renderRows();
    wrap.appendChild(ul);

    var bar = document.createElement("div");
    bar.style.cssText = TABX.bar;
    var mk = function(sym, dir) {
        var b = document.createElement("button");
        b.type = "button"; b.style.cssText = TABX.btn; b.textContent = sym;
        b.addEventListener("click", function(e) { e.preventDefault(); ul.focus(); move(dir); });
        return b;
    };
    bar.appendChild(mk("▲", -1));
    bar.appendChild(mk("▼",  1));
    wrap.appendChild(bar);
    return wrap;
};

// Build ONE merged, collapsed-by-default variable node: summary "<var> : N levels - reorder" (BOLD var
// name) -> one click opens the level list. `natural` is the column's level labels (from levelsCache):
// undefined = still loading, null = numeric/no-levels, array = factor levels (-> a buildVarBody list).
var makeVarNode = function(ui, v, axisLabel, natural) {
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
        natural === null      ? " — numeric (no levels)" :
                                (" : " + storedOrder(ui, v, natural).length + " levels – reorder"));
    sum.appendChild(caret); sum.appendChild(name); sum.appendChild(rest);
    varD.appendChild(sum);
    varD.addEventListener("toggle", function() {
        openState[vKey] = varD.open;
        caret.textContent = varD.open ? "▾" : "▸";
    });
    if (natural && natural.length) varD.appendChild(buildVarBody(ui, v, storedOrder(ui, v, natural)));
    return varD;
};

// Render the whole tree SYNCHRONOUSLY into $el (no deferred swap). A var whose levels aren't cached yet
// gets a "..." placeholder + a one-shot requestData that caches the levels and calls renderTree() again
// (synchronous this time -- no async swap can clobber an in-place edit). The `data-tabx-tree` marker lets
// the `updated` handler tell a jamovi $el re-render from a plain option write. Reorder MOVES update their
// list in place (buildVarBody) and never come through here.
var renderTree = function(ui) {
    if (!ui.levelOrderCtrl || !ui.levelOrder || !ui.row_vars) return;
    var rowV = utils.clone(ui.row_vars.value(), []);
    var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
    var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
    lastVarSig = JSON.stringify([rowV, colV, tabV]);
    var all = rowV.concat(colV).concat(tabV);
    reconcileLevelOrder(ui, all);

    var frag = document.createElement("div");
    frag.setAttribute("data-tabx-tree", "1");
    // Two-column grid: Row variables (col 1) | Column variables (col 2); Table variables below the
    // Row column (col 1, row 2) at the same width, the col-2 cell of that row left empty.
    frag.style.cssText = "display:grid;grid-template-columns:1fr 1fr;align-items:start;";
    var axes = [["Row variables", rowV, 1, 1], ["Column variables", colV, 2, 1],
                ["Table variables", tabV, 1, 2]];
    axes.forEach(function(ax) {
        var label = ax[0], vars = ax[1];
        if (vars.length === 0) return;
        var axD = makeTitledBox(TABX.axis, TABX.axisTitle, label);
        axD.style.gridColumn = String(ax[2]);
        axD.style.gridRow    = String(ax[3]);
        frag.appendChild(axD);
        vars.forEach(function(v) {
            var cached = (v in levelsCache) ? levelsCache[v] : undefined;
            axD.appendChild(makeVarNode(ui, v, label, cached === FETCHING ? undefined : cached));
            if (!(v in levelsCache)) {
                levelsCache[v] = FETCHING;                 // guard against duplicate in-flight fetches
                ui.levelOrderCtrl.requestData("column",
                    { columnName: v, properties: ["measureType", "levels"] })
                    .then(function(col) {
                        levelsCache[v] = (!col || col.measureType === "continuous")
                            ? null : col.levels.map(function(l) { return l.label; });
                        afterFetch(ui);
                    })
                    .catch(function() { levelsCache[v] = null; afterFetch(ui); });
            }
        });
    });
    if (all.length === 0) {
        var hint = document.createElement("div");
        hint.style.cssText = TABX.hint;
        hint.textContent = "Select row, column or table variables to reorder their levels.";
        frag.appendChild(hint);
    }
    var root = ui.levelOrderCtrl.$el[0];
    root.innerHTML = ""; root.appendChild(frag);
};

// ---- Phase 7g-iii: reference-level picker CustomControl (refPickerCtrl) --------------------
// One Material card per axis variable (row_vars under pct="row"/means, col_vars under pct="col"),
// each a SINGLE-SELECT list "[Total, ...levels in the reordered order...]" (radio dots; the selected
// one highlighted #b5caef). Stored by LABEL in the `refLevels` option, so a level reorder keeps the
// reference and just re-orders the list. A ref2 section (the odds-ratio 2nd reference) is shown only
// when OR is active. Distinct from the reorder tree: flat cards + radio dots, no Up/Down buttons, no
// collapsible tree. Shares levelsCache / requestData / storedOrder with the reorder tree.

// Signature that triggers a rebuild (vars + pct + color + OR + levelOrder). NOT refLevels / ref2 -- a
// pick is an in-place repaint, so the user's own click never rebuilds (mirrors the reorder tree).
var refSig = function(ui) {
    var rowV = utils.clone(ui.row_vars.value(), []);
    var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
    var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
    var pct    = ui.pct   ? ui.pct.value()   : "no";
    var colorV = ui.color ? ui.color.value() : "no";
    var ORv    = ui.OR    ? ui.OR.value()    : "no";
    var lo     = ui.levelOrder ? utils.clone(ui.levelOrder.value(), []) : [];
    return JSON.stringify([rowV, colV, tabV, pct, colorV, ORv, lo]);
};

var orIsActive = function(ui) {
    var colorV = ui.color ? ui.color.value() : "no";
    var ORv    = ui.OR    ? ui.OR.value()    : "no";
    return colorV === "OR" || ORv === "OR" || ORv === "OR_pct";
};

// The stored reference for variable `v` in refLevels ("" if the user has not picked one).
var refSelected = function(ui, v) {
    var arr = utils.clone(ui.refLevels.value(), []);
    for (var i = 0; i < arr.length; i++)
        if (arr[i].var === v) return (arr[i].ref == null ? "" : String(arr[i].ref));
    return "";
};

// Set/replace variable `v`'s reference entry in refLevels.
var writeRef = function(ui, v, refval) {
    var arr = utils.clone(ui.refLevels.value(), []);
    var found = false;
    for (var k = 0; k < arr.length; k++)
        if (arr[k].var === v) { arr[k] = { var: v, ref: refval }; found = true; break; }
    if (!found) arr.push({ var: v, ref: refval });
    ui.refLevels.setValue(arr);
};

// Drop refLevels entries whose var is not in the active axis (guarded setValue -> no loop): clears
// stale entries after a pct row<->col switch or a removed variable.
var reconcileRefLevels = function(ui, activeVars) {
    var cur = utils.clone(ui.refLevels.value(), []);
    var kept = [];
    for (var i = 0; i < cur.length; i++)
        if (activeVars.indexOf(cur[i].var) >= 0) kept.push(cur[i]);
    if (kept.length !== cur.length) ui.refLevels.setValue(kept);
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
    if (cached === null) {   // numeric col_var: reference is its own total, no drop-down
        var row = document.createElement("div"); row.style.cssText = TABX.refRow;
        var b1 = document.createElement("b"); b1.style.cssText = TABX.refName; b1.textContent = v;
        var nt = document.createElement("span"); nt.style.cssText = TABX.refNote;
        nt.textContent = "numeric — compared with its total";
        row.appendChild(b1); row.appendChild(nt);
        frag.appendChild(row);
        return;
    }
    var levels = storedOrder(ui, v, cached);
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
    var levels = (ref2var && (ref2var in levelsCache)) ? levelsCache[ref2var] : undefined;
    if (levels === FETCHING) levels = undefined;
    if (ref2var && !(ref2var in levelsCache)) {
        levelsCache[ref2var] = FETCHING;
        ui.refPickerCtrl.requestData("column",
            { columnName: ref2var, properties: ["measureType", "levels"] })
            .then(function(col) {
                levelsCache[ref2var] = (!col || col.measureType === "continuous")
                    ? null : col.levels.map(function(l) { return l.label; });
                afterFetch(ui);
            })
            .catch(function() { levelsCache[ref2var] = null; afterFetch(ui); });
    }
    var lvlChoices = (levels && levels.length)
        ? storedOrder(ui, ref2var, levels).map(function(l) { return { ref: l, label: l }; })
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

// Render the whole ref picker SYNCHRONOUSLY into $el (mirrors renderTree). refLevels/ref2 picks are
// in-place repaints and never come through here.
var renderRefPicker = function(ui) {
    if (!ui.refPickerCtrl || !ui.refLevels || !ui.row_vars) return;
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

    // levelOrderCtrl: build on create. On `updated`, re-render ONLY when the variable set changed OR
    // jamovi replaced our $el subtree (marker gone) -- a reorder MOVE fires `updated` (via setValue) with
    // the same vars + marker present, so it is SKIPPED and the in-place list update stands. This is what
    // fixes the "2nd click does nothing" bug (the old rebuild-and-swap clobbered the in-place edit).
    levelOrderCtrl_creating: function(ui) { renderTree(ui); },
    levelOrderCtrl_updated:  function(ui) {
        if (!ui.levelOrderCtrl || !ui.row_vars) return;
        var rowV = utils.clone(ui.row_vars.value(), []);
        var colV = ui.col_vars ? utils.clone(ui.col_vars.value(), []) : [];
        var tabV = ui.tab_vars ? utils.clone(ui.tab_vars.value(), []) : [];
        var sig = JSON.stringify([rowV, colV, tabV]);
        var root = ui.levelOrderCtrl.$el[0];
        var present = !!(root && root.firstChild && root.firstChild.getAttribute &&
                         root.firstChild.getAttribute("data-tabx-tree") === "1");
        if (sig === lastVarSig && present) return;
        renderTree(ui);
    },

    // refPickerCtrl: build on create. On `updated`, re-render ONLY when the signature (vars / pct /
    // color / OR / levelOrder) changed OR jamovi replaced our $el subtree (marker gone). A reference
    // PICK writes refLevels/ref2 -- not in the signature -- so it is SKIPPED and the in-place repaint
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

    // Keep the export button label + width in sync with the chosen format (Excel / HTML / markdown).
    export_format_changed: function(ui) {
        setExportLabel(ui);
        fixExportBtnWidth(ui);
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
