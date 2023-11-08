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
        title: qsTr("Output")
        GroupBox
        {
            CheckBox
			{
				name: "standardizedEstimate"
                id: stdest
				label: qsTr("Standardized estimates")
				checked: false
				RadioButtonGroup
				{
					name: "standardizedEstimateType"
					RadioButton { value: "all"; 	label: qsTr("All"); checked: true	}
					RadioButton { value: "latents"; label: qsTr("Latents")	}
					RadioButton { value: "noX"; 	label: qsTr("no X")		}
				}
			}
            CheckBox { label: qsTr("R-squared")                 ; name: "rSquared" }
            CheckBox { label: qsTr("Lavaan syntax")             ; name: "syntax" }
        }
            
        GroupBox
        {
            title: qsTr("Additional parameter estimates")
            CheckBox { label: qsTr("Total indirect effects");   name: "totalIndirectEffect"; checked: true }
            CheckBox { label: qsTr("Residual covariances");     name: "residualCovariance";    checked: true }
            CheckBox { label: qsTr("Path coefficients");        name: "pathCoefficient";    checked: true }
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
        text: qsTr("Estimation")
        GroupBox
        {
            RadioButtonGroup {
                title: qsTr("Error calculation")
                name: "errorCalculationMethod"
                enabled: estimator.currentValue == "default" || estimator.currentValue == "ml" || estimator.currentValue == "gls" || estimator.currentValue == "wls" || estimator.currentValue == "uls" || estimator.currentValue == "dwls"
                RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
                RadioButton { text: qsTr("Robust")    ; name: "robust" }
                RadioButton {
                    text: qsTr("Bootstrap")
                    name: "bootstrap"
                    enabled: !stdest.checked
                    IntegerField {
                        text: qsTr("Replications")
                        name: "bootstrapSamples"
                        defaultValue: 1000
                        min: 100
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
            CIField {
                text: qsTr("Confidence intervals")
                name: "ciLevel"
            }
        }
        
        GroupBox {
            Layout.fillWidth: true
            CheckBox{name: "standardizedVariable"; label: qsTr("Standardize variables before estimation"); checked: false}
            DropDown
			{
				name: "estimator"
				id: estimator
				label: qsTr("Estimator")
				values: [
                    { value: "default", label: qsTr("Auto"), checked: true    },
                    { value: "ml",      label: qsTr("ML")       },
                    { value: "gls",     label: qsTr("GLS")      },
                    { value: "wls",     label: qsTr("WLS")      },
                    { value: "uls",     label: qsTr("ULS")      },
                    { value: "dwls",    label: qsTr("DWLS")     },
                    { value: "mlf",     label: qsTr("MLF")      },
                    { value: "mlr",     label: qsTr("MLR")      }
                ]
			}
            RadioButtonGroup {
                title: qsTr("Missing data handling")
                name: "naAction"
                RadioButton { text: qsTr("Auto")                                ; name: "default" ; checked: true   }
                RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml"                      }
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise"                  }
                RadioButton { text: qsTr("Exclude cases pairwise")              ; name: "pairwise"                  }
            }
            DropDown
			{
				name: "emulation"
				label: qsTr("Emulation")
				values: [
					{ value: "lavaan",	label: qsTr("None") 	},
					{ value: "mplus",	label: qsTr("Mplus") 	},
					{ value: "eqs",		label: qsTr("EQS") 		}
				]
			}
        }
    } 
}


