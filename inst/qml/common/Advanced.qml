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

Section
{
  text: qsTr("Advanced")
	Group 
	{
		Layout.fillWidth: true
		RadioButtonGroup {
			title: qsTr("Missing value handling")
			name: "naAction"
			info: qsTr("Select the method for handling missing data in estimation")
			RadioButton { 
				text: qsTr("(FI)ML") 
				name: "fiml" 
				checked: true 
				info: qsTr("Use Full Information Maximum Likelihood to handle missing data")
			}
			RadioButton { 
				text: qsTr("Listwise")              
				name: "listwise"
				info: qsTr("Exclude cases with missing data (listwise deletion)")
			}
		}
		RadioButtonGroup {
			title: qsTr("Mimic")
			name: "emulation"
			info: qsTr("Select the software to emulate estimation behavior")
			RadioButton { 
				text: qsTr("Lavaan")  
				name: "lavaan"  
				checked: true 
				info: qsTr("Use Lavaan default estimation method")
			}
			RadioButton { 
				text: qsTr("Mplus") 
				name: "mplus" 
				info: qsTr("Emulate Mplus estimation methods")
			}
			RadioButton { 
				text: qsTr("EQS")   
				name: "eqs"   
				info: qsTr("Emulate EQS estimation methods")
			}
		}
  }
	Group 
	{
		Layout.fillWidth: true
		RadioButtonGroup {
			title: qsTr("Estimator")
			name: "estimator"
			info: qsTr("Choose the estimator for model fitting")
			RadioButton { 
				text: qsTr("Default") 
				name: "default"
				checked: true 
				info: qsTr("Use the default estimator based on the model and data")
			}
			RadioButton { 
				text: qsTr("ML")   
				name: "ml"
				info: qsTr("Maximum Likelihood estimator")
			}
			RadioButton { 
				text: qsTr("GLS")  
				name: "gls"
				info: qsTr("Generalized Least Squares estimator")
			}
			RadioButton { 
				text: qsTr("WLS")  
				name: "wls"
				info: qsTr("Weighted Least Squares estimator")
			}
			RadioButton { 
				text: qsTr("ULS")  
				name: "uls"
				info: qsTr("Unweighted Least Squares estimator")
			}
			RadioButton { 
				text: qsTr("DWLS") 
				name: "dwls"
				info: qsTr("Diagonally Weighted Least Squares estimator")
			}
		}
  }
}
