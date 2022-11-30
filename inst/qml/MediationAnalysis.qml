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
            GroupBox
            {
                CheckBox { label: qsTr("Standardized estimates") ;  name: "standardizedEstimate" }
                CheckBox { label: qsTr("Lavaan syntax")     ;       name: "syntax" }
                CheckBox { label: qsTr("R-squared")         ;       name: "rSquared" }
            }
            GroupBox
            {
                title: qsTr("Additional parameter estimates")
                CheckBox { label: qsTr("Total indirect effects");   name: "totalIndirectEffect"; checked: true }
                CheckBox { label: qsTr("Residual covariances");     name: "residualCovariance";    checked: true }
                CheckBox { label: qsTr("Path coefficients");        name: "pathCoefficient";    checked: true }
            }
        }
        GroupBox
        {
            CIField {
                text: qsTr("Confidence intervals")
                name: "ciLevel"
            }
            RadioButtonGroup {
                title: qsTr("Method")
                name: "errorCalculationMethod"
                enabled: estimator.currentValue == "default" || estimator.currentValue == "ml" || estimator.currentValue == "gls" || estimator.currentValue == "wls" || estimator.currentValue == "uls" || estimator.currentValue == "dwls" || estimator.currentValue == "pml"
                RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
                RadioButton { text: qsTr("Robust")    ; name: "robust" }
                RadioButton {
                    text: qsTr("Bootstrap")
                    name: "bootstrap"
                    IntegerField {
                        text: qsTr("Replications")
                        name: "bootstrapSamples"
                        defaultValue: 1000
                        min: 500
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
        GroupBox {
            Layout.fillWidth: true
            DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				values:
				[
					{ label: qsTr("FIML")				, value: "fiml"				},
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("Pairwise")			, value: "pairwise"			}
				]
			}
            RadioButtonGroup {
                title: qsTr("Emulation")
                name: "emulation"
                RadioButton { text: qsTr("None")  ; name: "lavaan"  ; checked: true }
                RadioButton { text: qsTr("Mplus") ; name: "mplus" }
                RadioButton { text: qsTr("EQS")   ; name: "eqs"   }
            }
        }
        GroupBox {
            Layout.fillWidth: true
            DropDown
			{
				name: "estimator"
				id: estimator
				label: qsTr("Estimator")
				values: [
                    { value: "default", label: qsTr("Auto")         },
                    { value: "ml",      label: qsTr("ML")           },
                    { value: "gls",     label: qsTr("GLS")          },
                    { value: "wls",     label: qsTr("WLS")          },
                    { value: "uls",     label: qsTr("ULS")          },
                    { value: "dwls",    label: qsTr("DWLS")         },
                    { value: "pml",     label: qsTr("PML")          },
                    { value: "mlm",     label: qsTr("Robust ML")    },
                    { value: "mlf",     label: qsTr("MLF")          },
                    { value: "mlr",     label: qsTr("MLR")          },
                    { value: "wlsm",    label: qsTr("Robust WLS")   },
                    { value: "ulsm",    label: qsTr("Robust ULS")   }

                ]

			}
        }
    }

}


