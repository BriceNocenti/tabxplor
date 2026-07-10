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

module.exports = {

    // Root view update. Bound explicitly via `events: update:` in .u.yaml; `view_updated` is the
    // jus-3.0 naming-convention alias -- keep both so initial sync fires whichever the build uses
    // (both are idempotent). Fixes the panel-hang from the jus-2.0 `context.clone` pattern.
    update:       onUpdate,
    view_updated: onUpdate,

    // Keep the refLevels picker in sync with the chosen row variables.
    onChange_row_vars: function(ui) {
        calcRefLevels(ui);
    },

    onChange_refLevels: function(ui) {
        updateLevelControls(ui);
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
