// Custom UI events for the jmvtab (Crosstables) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep
// it lean. See dev/tabxplor_1.4.0_jamovi_dev.md (§12 ref picker, §14 export) for the events API.
// jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

var exportLabels = { excel: "Excel", html: "HTML", md: "Markdown" };

var setExportLabel = function(ui) {
    if (!ui.export_format || !ui.exportExcel) return;
    var fmt = ui.export_format.value();
    ui.exportExcel.setPropertyValue("label", "Export to " + (exportLabels[fmt] || "Excel"));
};

// Reconcile the refLevels list to one {var, ref} row per selected row variable, preserving any
// level the user already chose for a variable still present.
var updateContrasts = function(ui, variableList) {
    var currentList = utils.clone(ui.refLevels.value(), []);
    var list3 = [];
    for (var i = 0; i < variableList.length; i++) {
        var found = null;
        for (var j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) { found = currentList[j]; break; }
        }
        list3.push(found !== null ? found : { var: variableList[i], ref: null });
    }
    ui.refLevels.setValue(list3);
};

// Bind each row's LevelSelector to its variable, so it lists that variable's levels.
var updateLevelControls = function(ui) {
    var dlist = utils.clone(ui.refLevels.value(), []);
    ui.refLevels.applyToItems(0, function(item, rowIndex, columnIndex) {
        if (columnIndex === 1 && dlist[rowIndex])
            item.setPropertyValue("variable", dlist[rowIndex].var);
    });
};

var calcRefLevels = function(ui) {
    if (!ui.row_vars || !ui.refLevels) return;
    updateContrasts(ui, utils.clone(ui.row_vars.value(), []));
    updateLevelControls(ui);
};

var onUpdate = function(ui) {
    setExportLabel(ui);
    calcRefLevels(ui);
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
    axisSum: "display:block;list-style:none;font-weight:600;padding:5px 8px;cursor:pointer;",
    varD:    "margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    varSum:  "display:block;list-style:none;padding:4px 8px;cursor:pointer;",
    body:    "padding:2px 8px 8px 8px;",
    list:    "list-style:none;margin:4px 0;padding:0;border:1px solid rgba(0,0,0,0.25);border-radius:3px;background:#fff;color:#000;max-height:180px;overflow-y:auto;outline:none;",
    item:    "padding:2px 8px;cursor:pointer;white-space:nowrap;",
    bar:     "display:flex;gap:6px;",
    btn:     "width:30px;height:22px;line-height:1;padding:0;cursor:pointer;",
    note:    "padding:4px 8px;opacity:0.65;font-style:italic;",
    hint:    "padding:8px;opacity:0.65;font-style:italic;"
};

// State persisting across rebuilds. levelsCache makes renderTree() SYNCHRONOUS after the first fetch --
// a deferred async swap was racing the user's in-place edits (the "2nd click does nothing, then all
// changes appear later" bug). lastVarSig + a root marker let the frequent `updated` event skip the
// rebuild during in-place reorder moves.
var openState = {};       // "<axis|var>:<key>" -> open bool
var levelsCache = {};     // var -> [labels] natural order | null (numeric/no-levels) | FETCHING sentinel
var FETCHING = {};
var lastVarSig = null;

var makeDetails = function(key, defOpen, boxStyle, sumStyle, summaryText) {
    var d = document.createElement("details");
    d.style.cssText = boxStyle;
    d.open = (key in openState) ? openState[key] : defOpen;
    var s = document.createElement("summary");
    s.style.cssText = sumStyle;                    // display:block hides the native marker
    var caret = document.createElement("span");    // explicit caret -> collapse affordance is CSS-proof
    caret.style.cssText = "display:inline-block;width:1.1em;";
    caret.textContent = d.open ? "▾" : "▸";
    s.appendChild(caret);
    s.appendChild(document.createTextNode(summaryText));
    d.addEventListener("toggle", function() {
        openState[key] = d.open;
        caret.textContent = d.open ? "▾" : "▸";
    });
    d.appendChild(s);
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
    var axes = [["Row variables", rowV], ["Column variables", colV], ["Table variables", tabV]];
    axes.forEach(function(ax) {
        var label = ax[0], vars = ax[1];
        if (vars.length === 0) return;
        var axD = makeDetails("axis:" + label, true, TABX.axis, TABX.axisSum, label);
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
                        renderTree(ui);
                    })
                    .catch(function() { levelsCache[v] = null; renderTree(ui); });
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

module.exports = {

    // Root view update. Bound explicitly via `events: update:` in .u.yaml; `view_updated` is the
    // jus-3.0 naming-convention alias -- keep both so initial sync fires whichever the build uses
    // (both are idempotent). Fixes the panel-hang from the jus-2.0 `context.clone` pattern.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box (row/col/tab) changed: re-sync the refLevels picker (row_vars only) AND the
    // level-reorder control. Shared by all three VariablesListBoxes (see .u.yaml `change` events).
    onChange_vars: function(ui) {
        calcRefLevels(ui);
        renderTree(ui);
    },

    onChange_refLevels: function(ui) {
        updateLevelControls(ui);
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

    // Keep the export button label in sync with the chosen format (Excel / HTML / Markdown).
    export_format_changed: function(ui) {
        setExportLabel(ui);
    },

    // Reset the export action button shortly after it is clicked, so a second export re-fires the
    // change event.
    exportExcel_changed: function(ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function() {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    }

};
