//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick          2.8
import QtQuick.Layouts  1.3
import JASP.Controls    1.0



Form
{
    VariablesForm
    {
        AvailableVariablesList
        {
            name: "availableVariables"
        }
        AssignedVariablesList
        {
			title: qsTr("Predictors")
            name:  "predictors"
            allowedColumns: []
        }
        AssignedVariablesList
        {
			title: qsTr("Mediators")
            name:  "mediators"
            allowedColumns: ["scale", "ordinal"]
        }
        AssignedVariablesList
        {
			title: qsTr("Outcome")
            name:  "outcomes"
            allowedColumns: ["scale", "ordinal"]
        }
        AssignedVariablesList
        {
			title: qsTr("Background confounders")
            name:  "confounds"
            allowedColumns: []
        }
    }

	Section
	{
        title: qsTr("Options")
        ColumnLayout {
			Group
            {
                CheckBox { label: qsTr("Standardized estimates") ;  name: "standardizedEstimate" }
                CheckBox { label: qsTr("Lavaan syntax")     ;       name: "syntax" }
                CheckBox { label: qsTr("R-squared")         ;       name: "rSquared" }
            }
			Group
            {
                title: qsTr("Additional parameter estimates")
                CheckBox { label: qsTr("Total indirect effects");   name: "totalIndirectEffect"; checked: true }
                CheckBox { label: qsTr("Residual covariances");     name: "residualCovariance";    checked: true }
                CheckBox { label: qsTr("Path coefficients");        name: "pathCoefficient";    checked: true }
            }
        }
		Group
        {
            CIField {
                text: qsTr("Confidence intervals")
                name: "ciLevel"
            }
            RadioButtonGroup {
                title: qsTr("Method")
                name: "errorCalculationMethod"
                RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
                RadioButton { text: qsTr("Robust")    ; name: "robust" }
                RadioButton {
                    text: qsTr("Bootstrap")
                    name: "bootstrap"
                    IntegerField {
                        text: qsTr("Replications")
                        name: "bootstrapSamples"
                        defaultValue: 1000
                        min: 200
                        max: 100000
                    }
                    DropDown {
                        label: qsTr("Type")
                        name: "bootstrapCiType"
                        values: [
                            { label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"   },
                            { label: qsTr("Percentile"),                value: "percentile"         },
                            { label: qsTr("Normal theory"),             value: "normalTheory"         }
                        ]
                    }
                }
            }
        }
    }

	Section {
        text: qsTr("Plots")
        CheckBox {
            text:   qsTr("Model plot")
            name:   "pathPlot"
            id:     pathPlot
            CheckBox { text: qsTr("Parameter estimates") ; name: "pathPlotParameter" }
            CheckBox { text: qsTr("Legend") ; name: "pathPlotLegend" }
        }
    }

	Section {
        text: qsTr("Advanced")
		Group {
            Layout.fillWidth: true
            RadioButtonGroup {
                title: qsTr("Missing value handling")
                name: "naAction"
                RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml" ; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise"             }
            }
            RadioButtonGroup {
                title: qsTr("Mimic")
                name: "emulation"
                RadioButton { text: qsTr("Lavaan")  ; name: "lavaan"  ; checked: true }
                RadioButton { text: qsTr("Mplus") ; name: "mplus" }
                RadioButton { text: qsTr("EQS")   ; name: "eqs"   }
            }
        }
		Group {
            Layout.fillWidth: true
            RadioButtonGroup {
                title: qsTr("Estimator")
                name: "estimator"
                RadioButton { text: qsTr("Default") ; name: "default"; checked: true }
                RadioButton { text: qsTr("ML")   ; name: "ml"       }
                RadioButton { text: qsTr("GLS")  ; name: "gls"      }
                RadioButton { text: qsTr("WLS")  ; name: "wls"      }
                RadioButton { text: qsTr("ULS")  ; name: "uls"      }
                RadioButton { text: qsTr("DWLS") ; name: "dwls"     }
            }
        }
    }

}


