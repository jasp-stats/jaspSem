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

Group
{
	CIField {
			text: qsTr("Confidence intervals")
			name: "ciLevel"
	}
	RadioButtonGroup {
		title: qsTr("Method")
		name: "errorCalculationMethod"
		RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
		RadioButton { text: qsTr("Robust")    ; name: "robust" }
		RadioButton {
			text: qsTr("Bootstrap")
			name: "bootstrap"
			IntegerField {
				text: qsTr("Replications")
				name: "bootstrapSamples"
				defaultValue: 1000
				min: 200
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
}