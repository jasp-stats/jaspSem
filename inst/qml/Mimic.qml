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
        }
        AssignedVariablesList
        {
            title: qsTr("Predictors")
            name:  "predictors"
            allowedColumns: []
        }
    }

	Section
	{
    title: qsTr("Options")
		Group
		{
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
			CheckBox { label: qsTr("Lavaan syntax")             ; name: "syntax" }
			CheckBox { label: qsTr("R-squared")                 ; name: "rSquared" }
			CheckBox { label: qsTr("Additional fit measures")   ; name: "additionalFitMeasures" }
		}

		Common.ErrorCalculation{}

  }

	Section 
	{
		text: qsTr("Plots")
		CheckBox 
		{ 
			text:   qsTr("Model plot")
			name:   "pathPlot"
			id:     pathPlot 
			CheckBox { text: qsTr("Parameter estimates") ; name: "pathPlotParameter" }
			CheckBox { text: qsTr("Legend") ; name: "pathPlotLegend" }
		}
  }
    
	Common.Advanced{}
    
}


