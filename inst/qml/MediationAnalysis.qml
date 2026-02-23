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
				title: qsTr("Predictors")
				name:  "predictors"
				id: predictors
				info: qsTr("One or multiple predictor variables predicting the mediators and the outcome variables.")
			}
			AssignedVariablesList
			{
				title: qsTr("Mediators")
				name:  "mediators"
				allowedColumns: ["scale", "ordinal"]
				allowTypeChange: true
				id: mediators
				info: qsTr("Variables through which the indirect effect of the predictors on the outcomes is hypothesized to flow.")
			}
			AssignedVariablesList
			{
				title: qsTr("Outcome")
				name:  "outcomes"
				allowedColumns: ["scale", "ordinal"]
				allowTypeChange: true
				id: outcomes
				info: qsTr("Variables predicted by the predictors and the mediators.")
			}
			AssignedVariablesList
			{
				title: qsTr("Background confounders")
				name:  "confounds"
				id: confounds
				info: qsTr("Variables explaining the predictors, mediators, and outcomes. Direct, indirect, and total effects are estimated conditional on these variables.")
			}
    }

	Section
	{
		title: qsTr("Options")
		info: qsTr("Options for standardization, additional parameter estimates, and model display.")
		ColumnLayout 
		{
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
				CheckBox { label: qsTr("Lavaan syntax");       name: "syntax";		info: qsTr("Display the lavaan syntax used to estimate the model.") }
				CheckBox { label: qsTr("R-squared");           name: "rSquared";	info: qsTr("Display the proportion of variance explained for each endogenous variable.") }
			}
			Group
      {
				title: qsTr("Additional parameter estimates")
				info: qsTr("Additional parameter estimate tables to display.")
				CheckBox { label: qsTr("Total indirect effects");   name: "totalIndirectEffect"; checked: true;	info: qsTr("Display total indirect effects summed across all mediators.") }
				CheckBox { label: qsTr("Residual covariances");     name: "residualCovariance";    checked: true;	info: qsTr("Display residual covariances between variables.") }
				CheckBox { label: qsTr("Path coefficients");        name: "pathCoefficient";    checked: true;	info: qsTr("Display the path coefficients of the model.") }
			}
		}
	  // create a string with all variables types to pass to the error calc elements
		// property bool ordinal: 
		Common.ErrorCalculation{}

	}

	Section 
	{
		text: qsTr("Plots")
		info: qsTr("Options for generating plots of the mediation model.")
		CheckBox {
			text:   qsTr("Model plot")
			name:   "pathPlot"
			id:     pathPlot
			info:   qsTr("Display a path diagram of the mediation model.")
			CheckBox { text: qsTr("Parameter estimates") ; name: "pathPlotParameter"; info: qsTr("Display parameter estimates on the path diagram.") }
			CheckBox { text: qsTr("Legend") ; name: "pathPlotLegend"; info: qsTr("Display a legend in the path diagram.") }
		}
  }

	Common.Advanced{}

}


