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
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls 
import JASP.Controls
import JASP.Widgets
import "./common" as Common

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
			allowedColumns: ["scale"]
			id: variables
		}
		AssignedVariablesList
		{
			name: "regressions"
			title: qsTr("Regressions")
			allowedColumns: ["scale"]
		}
		AssignedVariablesList
		{
			name: "categorical"
			id: categorical
			title: qsTr("Factor")
			allowedColumns: ["nominal"]
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Time-varying covariates")
			allowedColumns: ["scale"]
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
		title: qsTr("Estimation Options")
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

		Common.ErrorCalculation{}

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
		title: qsTr("Additional Output")
		Group
		{
			CheckBox { label: qsTr("Additional fit measures")   ; name: "additionalFitMeasures"	}
			CheckBox { label: qsTr("R-Squared")                 ; name: "rSquared"				}
			CheckBox
			{
				name: "standardizedEstimate"; label: qsTr("Standardized estimates");
				RadioButtonGroup
				{
					name: "standardizedEstimateType"
					RadioButton { value: "all"; 	label: qsTr("All"); checked: true	}
					RadioButton { value: "latents"; label: qsTr("Latents")	}
					RadioButton { value: "nox"; 	label: qsTr("Except exogenous covariates")		}
				}
			}
		}
		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovariance" 	}
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovariance"	}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "syntax" 				}
		}
	}

	Section
	{
		text: qsTr("Plots")
		Group
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

	Common.Advanced{}


	Group
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


