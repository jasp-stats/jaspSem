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
import JASP.Controls
import JASP

Group
{
	CIField {
			text: qsTr("Confidence intervals")
			name: "ciLevel"
			info: qsTr("Set the confidence level for the interval estimates")
	}
	RadioButtonGroup {
		title: qsTr("Method")
		name: "errorCalculationMethod"
		info: qsTr("Select the method for calculating standard errors and confidence intervals")
		RadioButton { 
			text: qsTr("Standard")  
			name: "standard" 
			checked: true 
			info: qsTr("Use standard maximum likelihood estimation for standard errors")
		}
		RadioButton { 
			text: qsTr("Robust")    
			name: "robust" 
			info: qsTr("Use robust estimation for standard errors")
		}
		RadioButton {
			text: qsTr("Bootstrap")
			name: "bootstrap"
			info: qsTr("Use bootstrap method for estimating standard errors and confidence intervals")
			IntegerField {
				text: qsTr("Replications")
				name: "bootstrapSamples"
				defaultValue: 1000
				min: 200
				max: 100000
				info: qsTr("Specify the number of bootstrap samples")
			}
			DropDown {
				label: qsTr("Type")
				name: "bootstrapCiType"
				values: [
						{ label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"   },
						{ label: qsTr("Percentile"),                value: "percentile"         },
						{ label: qsTr("Normal theory"),             value: "normalTheory"         }
				]
				info: qsTr("Select the type of bootstrap confidence interval to compute")
			}
		}
	}
}
