// Custom UI events for the jmvtabreg (Regressions) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep it
// lean. jus 3.0: use the GLOBAL `utils.clone` (the events `this` has no `.clone`, unlike jus 2.0).

var exportLabels = { excel: "Excel", html: "HTML", md: "Markdown" };

var setExportLabel = function(ui) {
    if (!ui.export_format || !ui.exportExcel) return;
    var fmt = ui.export_format.value();
    ui.exportExcel.setPropertyValue("label", "Export to " + (exportLabels[fmt] || "Excel"));
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

// jamovi's TextBox `width:` enum has no `auto` (caps at `largest`, ~200px). Widen `subtext` / `path`
// to fill their cell by clearing the fixed-width cap down to the input. Re-applied on each onUpdate.
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
    applyWtEnables(ui);
    stretchTextBox(ui, "subtext");
    stretchTextBox(ui, "path");
    renderRefPicker(ui);
};

// ---- Reference-level picker CustomControl (refPickerCtrl) ---------------------------------
// One Material line per FACTOR predictor = a bold name + a native <select> over its levels (the
// baseline the model contrasts against, default = the first level). Stored by LABEL in the hidden
// `refLevels` option, read by jmvtab_reg_ref_vector() -> tab_reg(reference =). Numeric predictors have
// no reference (a note). A reference change is reparametrized live from a cached fit (no refit).

var TABX = {
    refRow:  "display:grid;grid-template-columns:120px 1fr;align-items:center;gap:8px;width:66%;min-width:300px;box-sizing:border-box;padding:5px 8px;margin:4px 6px;border:1px solid rgba(0,0,0,0.12);border-radius:4px;background:rgba(0,0,0,0.03);",
    refName: "font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;",
    refSel:  "width:100%;min-width:0;box-sizing:border-box;padding:2px 4px;border:1px solid rgba(0,0,0,0.28);border-radius:3px;background:#fff;color:#000;cursor:pointer;",
    refNote: "opacity:0.6;font-style:italic;",
    hint:    "padding:8px;opacity:0.65;font-style:italic;"
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
    if (cached === null) {   // numeric predictor: no reference level
        var row = document.createElement("div"); row.style.cssText = TABX.refRow;
        var b1 = document.createElement("b"); b1.style.cssText = TABX.refName; b1.textContent = v;
        var nt = document.createElement("span"); nt.style.cssText = TABX.refNote;
        nt.textContent = "numeric — no reference level";
        row.appendChild(b1); row.appendChild(nt);
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

module.exports = {

    // Root view update. Bound via `events: update:`; `view_updated` is the jus-3.0 alias -- keep both.
    update:       onUpdate,
    view_updated: onUpdate,

    // A variable box changed: re-render the reference picker + re-apply the survey greying.
    onChange_vars: function(ui) {
        applyWtEnables(ui);
        renderRefPicker(ui);
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

    // Keep the export button label in sync with the chosen format.
    export_format_changed: function(ui) {
        setExportLabel(ui);
    },

    // Reset the export action button shortly after a click, so a second export re-fires the event.
    exportExcel_changed: function(ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function() {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    }

};
