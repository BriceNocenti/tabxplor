// Custom UI events for the jmvtab (Crosstables) analysis.
// NOTE: the jamovi compiler ships this file verbatim (comments included) to every user, so keep
// it lean. See dev/tabxplor_1.4.0_jamovi_dev.md for the events API.

module.exports = {

    // Reset the "Export to Excel" action button shortly after it is clicked, so a second export
    // re-fires the change event.
    exportExcel_changed: function(ui) {
        if (ui.exportExcel.value()) {
            setTimeout(function() {
                ui.exportExcel.setValue(false);
            }, 2000);
        }
    }

};
