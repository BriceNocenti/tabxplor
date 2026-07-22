// Custom UI events for the jmvtest (Export folder test) diagnostic analysis -- Last Phase o.
// The jamovi compiler ships this file verbatim (comments included), so keep it lean.
// An Action button's value goes true on click; the backend acts on it, then the JS must flip it back
// to false so a second press re-fires the change event (same pattern as jmvtab.js exportExcel_changed).

var onUpdate = function(ui) { };

module.exports = {

    update:       onUpdate,
    view_updated: onUpdate,

    write_detected_changed: function(ui) {
        if (ui.write_detected && ui.write_detected.value()) {
            setTimeout(function() { ui.write_detected.setValue(false); }, 2000);
        }
    },

    write_all_changed: function(ui) {
        if (ui.write_all && ui.write_all.value()) {
            setTimeout(function() { ui.write_all.setValue(false); }, 2000);
        }
    }

};
