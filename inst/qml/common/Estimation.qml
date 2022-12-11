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
//s
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
    {
        title: qsTr("Estimation")
		Group
		{
			DropDown
			{
				label: qsTr("Information matrix")
				name: "informationMatrix"
				values: [
					{ value: "expected", label: qsTr("Expected") },
					{ value: "observed", label: qsTr("Observed") }
				]
			}

			RadioButtonGroup
			{
				title: qsTr("Error calculation")
				name: "errorCalculationMethod"
				enabled: estimator.currentValue == "default" || estimator.currentValue == "ml" || estimator.currentValue == "gls" || estimator.currentValue == "wls" || estimator.currentValue == "uls" || estimator.currentValue == "dwls"
				RadioButton { value: "standard";	label: qsTr("Standard"); checked: true		}
				RadioButton { value: "robust";		label: qsTr("Robust")						}
				RadioButton
				{
					value: "bootstrap";	label: qsTr("Bootstrap")
					enabled: !stdest.checked
					IntegerField
					{
						name: "bootstrapSamples"
						label: qsTr("Bootstrap samples")
						fieldWidth: 60
						defaultValue: 1000
						min: 100
						max:100000
					}
					DropDown {
                        label: qsTr("Type")
                        name: "bootstrapCiType"
                        values: [
                            { label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"	},
                            { label: qsTr("Percentile"),                value: "percentile"         		},
                            { label: qsTr("Normal theory"),             value: "normalTheory"         		}
                        ]
                    }
				}
			}

			CIField {
				text: qsTr("Confidence intervals")
				name: "ciLevel"
			}


		}

		Group
		{
			CheckBox { name: "standardizedVariable";    label: qsTr("Standardize variables before estimation"); checked: false  }
			DropDown
			{
				name: "estimator"
				id: estimator
				label: qsTr("Estimator")
				values: [
                    { value: "default", label: qsTr("Auto")     },
                    { value: "ml",      label: qsTr("ML")       },
                    { value: "gls",     label: qsTr("GLS")      },
                    { value: "wls",     label: qsTr("WLS")      },
                    { value: "uls",     label: qsTr("ULS")      },
                    { value: "dwls",    label: qsTr("DWLS")     },
                    { value: "pml",     label: qsTr("PML")      },
                    { value: "mlf",     label: qsTr("MLF")      },
                    { value: "mlr",     label: qsTr("MLR")      }
                ]

			}

			DropDown
			{
				name: "modelTest"
				label: qsTr("Model test")
				values: [
					{ value: "default",					label: qsTr("Auto") 						},
					{ value: "standard",				label: qsTr("Standard")						},
					{ value: "satorraBentler",			label: qsTr("Satorra-Bentler")				},
					{ value: "yuanBentler",				label: qsTr("Yuan-Bentler")					},
					{ value: "meanAndVarianceAdjusted",	label: qsTr("Mean and Variance adjusted")	},
					{ value: "scaledAndShifted",		label: qsTr("Scaled and shifted")			},
					{ value: "bollenStine",				label: qsTr("Bootstrap (Bollen-Stine)")		}
				]
				enabled: estimator.currentValue == "default" || estimator.currentValue == "ml" || estimator.currentValue == "gls" || estimator.currentValue == "wls" || estimator.currentValue == "uls" || estimator.currentValue == "dwls"
			}

			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				values:
				[
					{ label: qsTr("Auto")               , value: "default"			},
					{ label: qsTr("FIML")				, value: "fiml"				},
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("Pairwise")			, value: "pairwise"			},
					{ label: qsTr("Two-stage")			, value: "twoStage"			},
					{ label: qsTr("Robust two-stage")	, value: "twoStageRobust"	},
					{ label: qsTr("Doubly robust")		, value: "doublyRobust"		},
				]
			}

			DropDown
			{
				name: "emulation"
				label: qsTr("Emulation")
				values: [
					{ value: "lavaan",	label: qsTr("None") 	},
					{ value: "mplus",	label: qsTr("Mplus") 	},
					{ value: "eqs",		label: qsTr("EQS") 		}
				]
			}
		}		
    }