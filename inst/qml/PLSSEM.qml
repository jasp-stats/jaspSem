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

Form
{

	columns: 1

	TabView
	{
		id: models
		name: "models"
		maximumItems: 1
		newItemName: qsTr("Model")
		optionKey: "name"

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeCSem; showLineNumber: true }
	}

	Section
	{
		title: qsTr("Model")


		Group
		{

			DropDown
			{
				id: grpvar
				name: "group"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
			}
		}
	}

	Section
	{
		title: qsTr("Estimation")

		Group 
		{
			CheckBox
			{
				name: "consistentPartialLeastSquares";		label: qsTr("Consistent partial least squares");	checked: true
			}
			DropDown
			{
				name: "innerWeightingScheme"
				label: qsTr("Inner weighting scheme")
				id: approachInner
				values: [
					{ value: "path", 		label: qsTr("Path")			},
					{ value: "centroid", 	label: qsTr("Centroid")		},
					{ value: "factorial", 	label: qsTr("Factorial")	}
				]
			}

			CheckBox
			{
				enabled: approachInner.currentValue != "path"
				name: "structuralModelIgnored"
				label: qsTr("Ignore structural model")
			}

			DropDown
			{
				name: "convergenceCriterion"
				label: qsTr("Convergence criterion")
				values: [
					{ value: "absoluteDifference",	label: qsTr("Absolute difference")	},
					{ value: "squaredDifference",		label: qsTr("Squared difference")	},
					{ value: "relativeDifference",	label: qsTr("Relative difference")	}
				]
			}

			DoubleField
			{
				name: "tolerance"
				label: qsTr("Tolerance")
				fieldWidth: 60
				defaultValue: 1e-5
				min: 0
			}
		}
		
		Group
		{
			title: qsTr("Error calculation method")
			RadioButtonGroup
			{
				name: "errorCalculationMethod"
				id: errorCalcMethod
				RadioButton { value: "none";		label: qsTr("None"); checked: true	}
				RadioButton {
							value: "bootstrap";	label: qsTr("Bootstrap"); checked: true
							IntegerField
							{
								name: "bootstrapSamples"
								label: qsTr("Samples")
								fieldWidth: 60
								defaultValue: 200
								min: 1
								// enabled: errorCalcMethod.value == "robust"
							}
							CIField
							{
								text: qsTr("Confidence intervals")
								name: "ciLevel"
								enabled: errorCalcMethod.value == "bootstrap"
							}
						}
				}
			RadioButtonGroup
			{
				visible: errorCalcMethod.value != "none"
				title: qsTr("Handling of inadmissibles")
				name: "handlingOfInadmissibles"
				RadioButton { value: "replace"; label: qsTr("Replace")	; checked: true	}
				RadioButton { value: "ignore"; 	label: qsTr("Ignore")					}
				RadioButton { value: "drop"; 	label: qsTr("Drop")						}
			}
			SetSeed {}
		}

		
	}

	Section
	{
		title: qsTr("Output")

		Group
		{
		  CheckBox { name: "rSquared";				label: qsTr("R-squared")				}
			CheckBox { name: "additionalFitMeasures";	label: qsTr("Additional fit measures")	}
			CheckBox { name: "mardiasCoefficient";		label: qsTr("Mardia's coefficient")		}
			CheckBox { name: "reliabilityMeasures";		label: qsTr("Reliability measures")		}
		}

		Group
		{
		  CheckBox { name: "observedIndicatorCorrelation";	label: qsTr("Observed indicator correlations")	}
			CheckBox { name: "impliedIndicatorCorrelation";		label: qsTr("Implied indicator correlations")	}
			CheckBox { name: "observedConstructCorrelation"; 	label: qsTr("Observed construct correlations")	}
			CheckBox { name: "impliedConstructCorrelation"; 	label: qsTr("Implied construct correlations")	}
		}

		CheckBox
		{
			name: "addConstructScores"
			text: qsTr("Add construct scores to data")
		}
	}

	Section
	{
		title: qsTr("Prediction")

		Group
		{
			CheckBox
			{	name: "endogenousIndicatorPrediction"
				label: qsTr("Predict endogenous indicator scores")
				id: prediction
			}

			IntegerField
			{
				name: "kFolds"
				label: qsTr("Cross-validation k-folds")
				fieldWidth: 60
				defaultValue: 10
				min: 2
				enabled: prediction.checked
			}

			IntegerField
			{
				name: "repetitions"
				label: qsTr("Repetitions")
				fieldWidth: 60
				defaultValue: 10
				min: 1
				enabled: prediction.checked
			}

			RadioButtonGroup
			{
				title: qsTr("Benchmark(s)")
				name: "benchmark"
				enabled: prediction.checked
				RadioButton { value: "none"; 	label: qsTr("None")	; checked: true	}
				RadioButton { value: "lm"; 		label: qsTr("Linear model")		}
				RadioButton { value: "GSCA"; 	label: qsTr("GSCA")				}
				RadioButton { value: "PCA";		label: qsTr("PCA")				}
				RadioButton { value: "MAXVAR";	label: qsTr("MAXVAR")			}
				RadioButton { value: "all";		label: qsTr("All")					}
			}
		}
	}
}
