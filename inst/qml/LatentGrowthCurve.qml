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
import JASP.Theme		1.0
import JASP.Widgets     1.0

Form
{

	VariablesForm
	{
		height: 420
		AvailableVariablesList
		{
			name: "availableVariables"
		}
		AssignedVariablesList
		{
			name: "variables"
			title: qsTr("Variables")
			suggestedColumns: ["scale"]
			id: variables
		}
		AssignedVariablesList
		{
			name: "regressions"
			title: qsTr("Regressions")
			suggestedColumns: ["scale"]
			allowedColumns: ["scale"]
		}
		AssignedVariablesList
		{
			name: "categorical"
			id: categorical
			title: qsTr("Factor")
			suggestedColumns: ["ordinal", "nominal"]
			allowedColumns: ["ordinal", "nominal", "nominalText"]
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Time-varying covariates")
			suggestedColumns: ["scale"]
			debug: true
			//listViewType: "RepeatedMeasures"
			//source: variables.name
		}
	}

	AssignedVariablesList
	{
		name: "timings"
		title: qsTr("Timings")
		source: "variables"
		draggable: false
		preferredWidth: form.width * 2 / 5
		preferredHeight: 140

		rowComponent: DoubleField {
			name: "timing"
			negativeValues: true
			defaultValue: rowIndex
			useExternalBorder: false
		}
	}

	Section
	{
		title: qsTr("Model")
		Group
		{
			title: qsTr("Growth curve shape")
			CheckBox
			{
				label: qsTr("Intercept")
				name: "intercept"
				id: intercept
				checked: true
			}
			CheckBox
			{
				label: qsTr("Linear")
				name: "linear"
				id: linear
				enabled: intercept.checked
				checked: true
			}
			CheckBox
			{
				label: qsTr("Quadratic")
				name: "quadratic"
				id: quadratic
				enabled: linear.checked
				checked: if (!linear.checked) return false
			}
			CheckBox
			{
				label: qsTr("Cubic")
				name: "cubic"
				id: cubic
				enabled: quadratic.checked
				checked: if (!quadratic.checked) return false
			}
		}

		Group
		{
			CheckBox
			{
				label: qsTr("Covarying latent curve")
				name: "covaryingLatentCurve"
				enabled: linear.checked
				checked: true
			}
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
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovariance" 	}
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovariance"	}
        }
		GroupBox
		{
			CheckBox { label: qsTr("Additional Fit Measures")   ; name: "additionalFitMeasures"	}
			CheckBox { label: qsTr("R-Squared")                 ; name: "rSquared"				}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "syntax" 				}
		}
	}

	Section
	{
		text: qsTr("Plots")
		GroupBox
		{
			title: "Plots"
			ColorPalette{}
			CheckBox {
				text: qsTr("Curve plot")
				name: "curvePlot"
				DropDown {
					name: "curvePlotCategorical"
					label: "Colour lines by"
					source: "categorical"
					addEmptyValue: true
					enabled: categorical.count > 0
				}

				IntegerField {
					name: "curvePlotMaxLines"
					text: "Maximum number of lines"
					defaultValue: 150
					negativeValues: false
				}
			}
			CheckBox { text: qsTr("Misfit plot")    ; name: "misfitPlot"   ; debug: true }
			CheckBox
			{
				text: qsTr("Model plot")
				name: "pathPlot"
				CheckBox { text: qsTr("Show parameters") ; name: "pathPlotParameter"	}
				CheckBox { text: qsTr("Show means")      ; name: "pathPlotMean"			}
			}
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
            CheckBox{name: "standardizedVariable"; id: stdvar; label: qsTr("Standardize variables before estimation"); checked: false}
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
            DropDown
			{
				name: "modelTest"
				label: qsTr("Model test")
				values: [
					{ value: "default",					label: qsTr("Auto") 						},
					{ value: "standard",				label: qsTr("Standard")						},
					{ value: "satorraBentler",			label: qsTr("Satorra-Bentler")				},
					{ value: "yuanBentler",				label: qsTr("Yuan-Bentler")					},
					{ value: "meanAndVarianceAdjusted",	label: qsTr("Mean and Variance adjusted")	},
					{ value: "scaledAndShifted",		label: qsTr("Scaled and shifted")			},
					{ value: "bollenStine",				label: qsTr("Bootstrap (Bollen-Stine)")		}
				]
				enabled: estimator.currentValue == "default" || estimator.currentValue == "ml" || estimator.currentValue == "gls" || estimator.currentValue == "wls" || estimator.currentValue == "uls" || estimator.currentValue == "dwls"
            }
            RadioButtonGroup {
                title: qsTr("Missing data handling")
                name: "naAction"
				RadioButton { text: qsTr("Auto")                                ; name: "default" ; checked: true   }
                RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml"					}
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise"            	}
                RadioButton { text: qsTr("Exclude cases pairwise")              ; name: "pairwise"             	}
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

		GroupBox
		{
			title: qsTr("Options")
			debug: true
			CheckBox { text: qsTr("Fix manifest intercepts to zero") ; name: "manifestInterceptFixedToZero" 	}
			CheckBox { text: qsTr("Fix latent intercepts to zero")   ; name: "latentInterceptFixedToZero"		; checked: true }
			CheckBox { text: qsTr("Omit residual single indicator")  ; name: "residualSingleIndicatorOmitted"	; checked: true }
			CheckBox { text: qsTr("Residual variances")              ; name: "residualVariance"           		; checked: true }
			CheckBox { text: qsTr("Correlate exogenous latents")     ; name: "exogenousLatentCorrelation"   	; checked: true }
			CheckBox { text: qsTr("Add thresholdds")                 ; name: "threshold"               			; checked: true }
			CheckBox { text: qsTr("Add scalings parameters")         ; name: "scalingParameter"        			; checked: true }
			CheckBox { text: qsTr("Correlate dependent variables")   ; name: "dependentCorrelation" 			; checked: true }
		}
	}

	Section
	{
		text: qsTr("Multigroup LGCM")
		debug: true
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "group";
			showVariableTypeIcon: true;
			addEmptyValue: true;
		} // No model or syncModels: it takes all variables per default
	}
}


