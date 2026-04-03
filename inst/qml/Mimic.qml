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
import JASP
import "./common" as Common



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
            title: qsTr("Indicators")
            name:  "indicators"
            allowedColumns: ["scale", "ordinal"]
            info: qsTr("Observed indicator variables that define the latent variable in the MIMIC model.")
        }
        AssignedVariablesList
        {
            title: qsTr("Predictors")
            name:  "predictors"
            allowedColumns: []
            info: qsTr("Observed predictor variables (causes) that directly influence the latent variable.")
        }
    }

	Section
	{
    title: qsTr("Options")
    info: qsTr("Options for standardization, model syntax display, and additional output.")
		Group
		{
			CheckBox
			{
				name: "standardizedEstimate"; label: qsTr("Standardized estimates");
				info: qsTr("Standardize all variables (mean = 0, sd = 1) before estimation.")
				RadioButtonGroup
				{
						name: "standardizedEstimateType"
						info: qsTr("Type of standardization.")
						RadioButton { value: "all"; 	label: qsTr("All"); checked: true;	info: qsTr("Standardize based on variances of both observed and latent variables.") }
						RadioButton { value: "latents"; label: qsTr("Latents");				info: qsTr("Standardize based on latent variable variances only.") }
						RadioButton { value: "nox"; 	label: qsTr("Except exogenous covariates");	info: qsTr("Standardize excluding exogenous covariates.") }
				}
			}
			CheckBox { label: qsTr("Lavaan syntax")             ; name: "syntax";					info: qsTr("Display the lavaan syntax used to estimate the model.") }
			CheckBox { label: qsTr("R-squared")                 ; name: "rSquared";					info: qsTr("Display the proportion of variance explained for each endogenous variable.") }
			CheckBox { label: qsTr("Additional fit measures")   ; name: "additionalFitMeasures";	info: qsTr("Display additional fit indices for evaluating model fit (e.g., CFI, TLI, RMSEA, SRMR).") }
		}

		Common.ErrorCalculation{}

  }

	Section 
	{
		text: qsTr("Plots")
		info: qsTr("Options for generating plots of the MIMIC model.")
		CheckBox 
		{ 
			text:   qsTr("Model plot")
			name:   "pathPlot"
			id:     pathPlot
			info:   qsTr("Display a path diagram of the MIMIC model.")
			CheckBox { text: qsTr("Parameter estimates") ; name: "pathPlotParameter"; info: qsTr("Display parameter estimates on the path diagram.") }
			CheckBox { text: qsTr("Legend") ; name: "pathPlotLegend"; info: qsTr("Display a legend in the path diagram.") }
		}
  }
    
	Common.Advanced{}
    
}


