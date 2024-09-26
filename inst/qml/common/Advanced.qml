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
import JASP
import JASP.Controls
import JASP.Widgets
import QtQuick.Controls  as QTCONTROLS

Section
{
  text: qsTr("Advanced")
	Group 
	{
		Layout.fillWidth: true
		RadioButtonGroup {
			title: qsTr("Missing value handling")
			name: "naAction"
			RadioButton { text: qsTr("(FI)ML") ; name: "fiml" ; checked: true }
			RadioButton { text: qsTr("Listwise")              ; name: "listwise"             }
		}
		RadioButtonGroup {
			title: qsTr("Mimic")
			name: "emulation"
			RadioButton { text: qsTr("Lavaan")  ; name: "lavaan"  ; checked: true }
			RadioButton { text: qsTr("Mplus") ; name: "mplus" }
			RadioButton { text: qsTr("EQS")   ; name: "eqs"   }
		}
  }
	Group 
	{
		Layout.fillWidth: true
		RadioButtonGroup {
			title: qsTr("Estimator")
			name: "estimator"
			RadioButton { text: qsTr("Default") ; name: "default"; checked: true }
			RadioButton { text: qsTr("ML")   ; name: "ml"       }
			RadioButton { text: qsTr("GLS")  ; name: "gls"      }
			RadioButton { text: qsTr("WLS")  ; name: "wls"      }
			RadioButton { text: qsTr("ULS")  ; name: "uls"      }
			RadioButton { text: qsTr("DWLS") ; name: "dwls"     }
		}
  }
}