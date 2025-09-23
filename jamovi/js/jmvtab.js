//
// htmltools::includeScript(# bootstrap jv
//   system.file("rmd/h/bootstrap/js/bootstrap.min.js", package = "rmarkdown"),
// )
// htmltools::includeScript(# bootstrap jv
//   system.file("rmd/h/bootstrap/shim/html5shiv.min.js", package = "rmarkdown"),
// )
// htmltools::includeScript(# bootstrap jv
//   system.file("rmd/h/bootstrap/shim/respond.min.js", package = "rmarkdown"),
// )
//
// htmltools::includeScript(# html_dependency_kePrint (popover)
//   system.file("kePrint-0.0.1/kePrint.js", package = "kableExtra")
// )
//
// htmltools::includeScript( # jquery
//   system.file("lib/3.6.0/jquery-3.6.0.min.js", package = "jquerylib")
// )


module.exports = {

exportExcel_clicked(ui, event) {
  console.log("exportExcel button clicked");
  //const current = ui.doExportExcel.value();
  // ui.doExportExcel.setValue(!current);  // toggle to force analysis run
}

//  exportExcel_clicked(ui, event) {
//    // Toggle Bool to force rerun of backend R
//    const current = ui.doExportExcel.value();
//    ui.doExportExcel.setValue(!current);
//
//    // Reset Action button so it can be clicked again
//    setTimeout(() => {
//      ui.exportExcel.setValue(false);
//    }, 100);
//  }


//  tab_vars_changed(ui) { // (ui, event)
//    // let tab_vars = ui.tab_vars.value(); // let : block-scoped local variables
//    // let size = Object.keys(tab_vars).length;
//
//    let tab_vars = utils.clone(ui.tab_vars.value(), []);
//    let size = tab_vars.length
//
//     // alert(size.toString());
//
//    if (size == 0) {
//       // // jamovi/client/analysisui/optioncontrol.js
//       // this.setEnabled(true);
//       // this.setPropertyValue('enable', true);
//
//
//        ui.comp.tab.setAttribute("disabled", true);
//        ui.comp.tab.setAttribute("enabled", false);
//        ui.comp.tab.setEnabled(false)
//        //ui.comp.tab.setPropertyValue('enable', false);
//
//        // ui.comp.all.setAttribute("disabled", true);
//        // ui.comp.setValue(0.707)
//
//    } else { // size > 0
//        ui.comp.tab.removeAttribute("disabled");
//        ui.comp.tab.setAttribute("enabled", true);
//        ui.comp.tab.setEnabled(true)
//        //ui.comp.tab.setPropertyValue('enable', true);
//        // ui.comp.all.removeAttribute("disabled");
//    }
//
//  } //,

};










// // Exemples
//
// if (false) {
//   // jamovi/js/anova.js
//   'use strict';
//
// module.exports = {
//
//     view_updated: function(ui) {
//         this.calcModelTerms(ui);
//         this.filterModelTerms(ui);
//         this.updateModelLabels(ui.emMeans, _('Term {0}'));
//     },
//
//     factors_changed: function(ui) {
//         this.calcModelTerms(ui);
//     },
//
//     modelTerms_changed: function(ui) {
//         this.filterModelTerms(ui);
//     },
//
//     emMeansSupplier_updated: function(ui) {
//         this.calcMarginalMeansSupplier(ui);
//     },
//
//     modelSupplier_updated: function(ui) {
//         let variableList = utils.clone(ui.factors.value(), []);
//         ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));
//     },
//
//     postHocSupplier_updated: function(ui) {
//         let termsList = utils.clone(ui.modelTerms.value(), []);
//         ui.postHocSupplier.setValue(utils.valuesToItems(termsList, FormatDef.term));
//     },
//
//     emMeansSupplier_changed: function(ui) {
//         let values = utils.itemsToValues(ui.emMeansSupplier.value());
//         utils.checkValue(ui.emMeans, 2, values, FormatDef.variable);
//     },
//
//     postHocSupplier_changed: function(ui) {
//         let values = utils.itemsToValues(ui.postHocSupplier.value());
//         utils.checkValue(ui.postHoc, true, values, FormatDef.term);
//     },
//
//     emMeans_listItemsChanged: function(ui) {
//         this.updateModelLabels(ui.emMeans, _('Term {0}'));
//     },
//
//     filterModelTerms: function(ui) {
//         var termsList = utils.clone(ui.modelTerms.value(), []);
//
//         //Remove common terms
//         var termsDiff = this.findChanges("currentList", termsList, true, FormatDef.term);
//         var changed = false;
//         if (termsDiff.removed.length > 0 && termsList !== null) {
//             var itemsRemoved = false;
//             for (var i = 0; i < termsDiff.removed.length; i++) {
//                 var item = termsDiff.removed[i];
//                 for (var j = 0; j < termsList.length; j++) {
//                     if (FormatDef.term.contains(termsList[j], item)) {
//                         termsList.splice(j, 1);
//                         j -= 1;
//                         itemsRemoved = true;
//                     }
//                 }
//             }
//
//             if (itemsRemoved)
//                 changed = true;
//         }
//         /////////////////////
//
//         //Sort terms
//         if (utils.sortArraysByLength(termsList))
//             changed = true;
//         ////////////
//
//         if (changed)
//             ui.modelTerms.setValue(termsList);
//
//         ui.postHocSupplier.setValue(utils.valuesToItems(termsList, FormatDef.term));
//     },
//
//     calcMarginalMeansSupplier: function(ui) {
//
//         let b1 = utils.clone(ui.factors.value(), []);
//         b1 = utils.valuesToItems(b1, FormatDef.variable);
//
//         if (ui.emMeansSupplier)
//             ui.emMeansSupplier.setValue(b1);
//     },
//
//     updateModelLabels: function(list, blockName) {
//         list.applyToItems(0, (item, index) => {
//             item.controls[0].setPropertyValue("label", blockName.replace('{0}', (index + 1) ));
//         });
//     },
//
//     calcModelTerms: function(ui) {
//         var variableList = utils.clone(ui.factors.value(), []);
//
//         ui.modelSupplier.setValue(utils.valuesToItems(variableList, FormatDef.variable));
//
//         this.calcMarginalMeansSupplier(ui);
//
//         var varsDiff = this.findChanges("variableList", variableList, true, FormatDef.variable);
//         var termsList = utils.clone(ui.modelTerms.value(), []);
//
//         var termsChanged = false;
//         for (var i = 0; i < varsDiff.removed.length; i++) {
//             for (var j = 0; j < termsList.length; j++) {
//                 if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
//                     termsList.splice(j, 1);
//                     termsChanged = true;
//                     j -= 1;
//                 }
//             }
//         }
//
//         termsList = utils.getCombinations(varsDiff.added, termsList);
//         termsChanged = termsChanged || varsDiff.added.length > 0;
//
//         if (termsChanged)
//             ui.modelTerms.setValue(termsList);
//
//         this.updateContrasts(ui, variableList);
//     },
//
//     updateContrasts: function(ui, variableList) {
//         let value = ui.contrasts.value();
//         var currentList = utils.clone(value, []);
//
//         var list3 = [];
//         for (let i = 0; i < variableList.length; i++) {
//             let found = null;
//             for (let j = 0; j < currentList.length; j++) {
//                 if (currentList[j].var === variableList[i]) {
//                     found = currentList[j];
//                     break;
//                 }
//             }
//             if (found === null)
//                 list3.push({ var: variableList[i], type: "none" });
//             else
//                 list3.push(found);
//         }
//
//         let oldLength = value === null ? 0 : value.length;
//
//         let changed = oldLength !== list3.length || JSON.stringify(value) !== JSON.stringify(list3);
//
//         if (changed)
//             ui.contrasts.setValue(list3);
//     }
//
// };
//
//
//
//
// // jamovi/js/cfa.events.js
// const events = {
//
//     update: function(ui) {
//         updateModelLabels(ui, this);
//         calcModelTerms(ui, this);
//     },
//
//     onEvent_test_listItemsAdded: function(ui, data) {
//         updateModelLabels(ui, this);
//         calcModelTerms(ui, this);
//         setTimeout(() => {
//             data.item.controls[0].$input.focus();
//         }, 0);
//     },
//
//     onEvent_test_listItemsChanged: function(ui) {
//         updateModelLabels(ui, this);
//         calcModelTerms(ui, this);
//     },
//
//     onChange_resCovSupplier: function(ui) {
//         let values = this.itemsToValues(ui.resCovSupplier.value());
//         this.checkPairsValue(ui.resCov, values);
//     },
//
//     onUpdate_resCovSupplier: function(ui) {
//         calcModelTerms(ui, this);
//     },
//
//     onEvent_factorNameChange : function(ui) {
//         updateModelLabels(ui, this);
//     }
// };
//
// const updateModelLabels = function(ui, context) {
//     let list = ui.factors.applyToItems(0, (item, index) => {
//         let value = item.controls[0].value();
//         if ( ! value || value.trim() === '')
//             item.controls[0].setValue(_('Factor {0}').replace('{0}', (index + 1)) );
//     });
// };
//
// const calcModelTerms = function(ui, context) {
//
//     let factorList = context.clone(ui.factors.value(), []);
//
//     let variables = [];
//     for (let i = 0 ; i < factorList.length; i++) {
//         let vars = factorList[i].vars;
//         if (vars) {
//             for (let y = 0; y < vars.length; y++) {
//                 let variable = vars[y];
//                 if (variable) {
//                     let found = false;
//                     for (let j = 0; j < variables.length; j++) {
//                         if (variables[j] === variable) {
//                             found = true;
//                             break;
//                         }
//                     }
//                     if (found == false)
//                         variables.push(variable);
//                 }
//             }
//         }
//     }
//
//     ui.resCovSupplier.setValue(context.valuesToItems(variables, FormatDef.variable));
// };
//
// module.exports = events;
//
//
//
// }




//    module.exports = {
//
//        // event handlers and functions are defined here
//
//        // this is an example of an event handler
//        view_loaded: function(ui, event) {
//            // do something
//        },
//
//        // this is another example of an event handler
//        ttestType_changed: function(ui, event) {
//            let value = this.calculateValue();
//            // do something
//        },
//
//        // this is an example of an auxiliary function
//        calculateValue: function() {
//            // do something
//        }
//
//       ui.view.model.options.beginEdit();
//       ui.figWidth.setValue(400);
//       ui.figHeight.setValue(300);
//       ui.view.model.options.endEdit();
//
//    };
