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
			info: qsTr("List of all available variables for analysis")
		}
		AssignedVariablesList
		{
			name: "variables"
			title: qsTr("Variables")
			allowedColumns: ["scale"]
			id: variables
			info: qsTr("Variables to include in the latent growth curve model")
		}
		AssignedVariablesList
		{
			name: "regressions"
			title: qsTr("Regressions")
			allowedColumns: ["scale"]
			info: qsTr("Variables to use as regressors in the model")
		}
		AssignedVariablesList
		{
			name: "categorical"
			id: categorical
			title: qsTr("Factor")
			allowedColumns: ["nominal"]
			info: qsTr("Categorical variables acting as factors")
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Time-varying covariates")
			allowedColumns: ["scale"]
			debug: true
			info: qsTr("Time-varying covariates in the model")
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
		info: qsTr("Variable timing")

		rowComponent: DoubleField {
			name: "timing"
			negativeValues: true
			defaultValue: rowIndex
			useExternalBorder: false
			info: qsTr("Specify the timing for the variable")
		}
	}

	Section
	{
		title: qsTr("Estimation Options")
		info: qsTr("Options for configuring model estimation")
		Group
		{
			title: qsTr("Growth curve shape")
			info: qsTr("Select components to include in the growth curve")
			CheckBox
			{
				label: qsTr("Intercept")
				name: "intercept"
				id: intercept
				checked: true
				info: qsTr("Include an intercept in the model")
			}
			CheckBox
			{
				label: qsTr("Linear")
				name: "linear"
				id: linear
				enabled: intercept.checked
				checked: true
				info: qsTr("Include a linear component in the model")
			}
			CheckBox
			{
				label: qsTr("Quadratic")
				name: "quadratic"
				id: quadratic
				enabled: linear.checked
				checked: if (!linear.checked) return false
				info: qsTr("Include a quadratic component in the model")
			}
			CheckBox
			{
				label: qsTr("Cubic")
				name: "cubic"
				id: cubic
				enabled: quadratic.checked
				checked: if (!quadratic.checked) return false
				info: qsTr("Include a cubic component in the model")
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
				info: qsTr("Include covariance between latent growth components")
			}
		}
	}

	Section
	{
		title: qsTr("Additional Output")
		info: qsTr("Options for additional output")
		Group
		{
			CheckBox { label: qsTr("Additional fit measures"); name: "additionalFitMeasures"	; info: qsTr("Display additional model fit measures") }
			CheckBox { label: qsTr("R-Squared")                 ; name: "rSquared"				; info: qsTr("Display R-squared values for variables") }
			CheckBox
			{
				name: "standardizedEstimate"; label: qsTr("Standardized estimates"); info: qsTr("Include standardized estimates in the output")
				RadioButtonGroup
				{
					name: "standardizedEstimateType"
					info: qsTr("Select the type of standardized estimates to include")
					RadioButton { value: "all"; 	label: qsTr("All"); checked: true; info: qsTr("Include all standardized estimates") }
					RadioButton { value: "latents"; label: qsTr("Latents"); info: qsTr("Include standardized estimates for latent variables only") }
					RadioButton { value: "nox"; 	label: qsTr("Except exogenous covariates"); info: qsTr("Exclude standardized estimates for exogenous covariates") }
				}
			}
		}
		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovariance" 	; info: qsTr("Display the implied covariance matrix") }
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovariance"	; info: qsTr("Display the residual covariance matrix") }
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "syntax" 				; info: qsTr("Show the lavaan model syntax") }
		}
	}

	Section
	{
		text: qsTr("Plots")
		info: qsTr("Options for generating plots")
		Group
		{
			title: "Plots"
			info: qsTr("Configure plot options for the analysis")
			ColorPalette{ info: qsTr("Select a color palette for the plots") }
			CheckBox {
				text: qsTr("Curve plot")
				name: "curvePlot"
				info: qsTr("Generate a curve plot of the growth trajectories")
				DropDown {
					name: "curvePlotCategorical"
					label: "Colour lines by"
					source: "categorical"
					addEmptyValue: true
					enabled: categorical.count > 0
					info: qsTr("Select a categorical variable to color the curves")
				}

				IntegerField {
					name: "curvePlotMaxLines"
					text: "Maximum number of lines"
					defaultValue: 150
					negativeValues: false
					info: qsTr("Set the maximum number of lines to display in the curve plot")
				}
			}
			CheckBox { text: qsTr("Misfit plot")    ; name: "misfitPlot"   ; debug: true }
			CheckBox
			{
				text: qsTr("Model plot")
				name: "pathPlot"
				info: qsTr("Generate a path diagram of the model")
				CheckBox { text: qsTr("Show parameters") ; name: "pathPlotParameter"; info: qsTr("Display parameter estimates in the model plot") }
				CheckBox { text: qsTr("Show means")      ; name: "pathPlotMean"; info: qsTr("Display means in the model plot") }
			}
		}
	}

	Common.Advanced{}


	Group
	{
		title: qsTr("Options")
		debug: true
		info: qsTr("Additional modeling options")
		CheckBox { text: qsTr("Fix manifest intercepts to zero") ; name: "manifestInterceptFixedToZero" 	; info: qsTr("Fix manifest intercepts to zero") }
		CheckBox { text: qsTr("Fix latent intercepts to zero")   ; name: "latentInterceptFixedToZero"		; checked: true ; info: qsTr("Fix latent intercepts to zero") }
		CheckBox { text: qsTr("Omit residual single indicator")  ; name: "residualSingleIndicatorOmitted"	; checked: true ; info: qsTr("Omit residuals for single indicators") }
		CheckBox { text: qsTr("Residual variances")              ; name: "residualVariance"           		; checked: true ; info: qsTr("Include residual variances in the model") }
		CheckBox { text: qsTr("Correlate exogenous latents")     ; name: "exogenousLatentCorrelation"   	; checked: true ; info: qsTr("Allow exogenous latent variables to correlate") }
		CheckBox { text: qsTr("Add thresholds")                  ; name: "threshold"               			; checked: true ; info: qsTr("Include thresholds in the model") }
		CheckBox { text: qsTr("Add scaling parameters")          ; name: "scalingParameter"        			; checked: true ; info: qsTr("Include scaling parameters in the model") }
		CheckBox { text: qsTr("Correlate dependent variables")   ; name: "dependentCorrelation" 			; checked: true ; info: qsTr("Allow dependent variables to correlate") }
	}
	

	Section
	{
		text: qsTr("Multigroup LGCM")
		debug: true
		info: qsTr("Options for multigroup latent growth curve modeling")
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "group";
			showVariableTypeIcon: true;
			addEmptyValue: true;
			info: qsTr("Select a grouping variable that is ideally nominally scaled")
		} // No model or syncModels: it takes all variables per default
	}
}


