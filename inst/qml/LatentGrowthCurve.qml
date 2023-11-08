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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
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

	Common.Estimation {	}

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


