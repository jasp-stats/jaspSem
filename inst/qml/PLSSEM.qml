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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import QtQuick.Controls 2.12
import JASP.Controls	1.0
import JASP.Widgets 1.0
import JASP				1.0

Form
{

	columns: 1

	TabView
	{
		id: models
		name: "models"
		maximumItems: 9
		newItemName: qsTr("Model 1")
		optionKey: "name"

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeCSem }
	}

	Section
	{
		title: qsTr("Model")


		Group
		{
			CheckBox
			{
				enabled: approachWeigths.currentValue == "PLS-PM"
				name: "structuralModelIgnored"
				label: qsTr("Ignore structural model")
			}

			CheckBox
			{
				name: "compositeCorrelationDisattenuated";		label: qsTr("Disattenuate composite correlations");	checked: true
				DropDown
				{
					name: "correctionFactor"
					label: qsTr("Approach correction factors")
					values: [
						{ value: "squaredEuclidean", 		label: qsTr("Squared Euclidean distance")	},
						{ value: "weightedEuclidean", 		label: qsTr("Weighted Euclidean distance")	},
						{ value: "fisherTransformed", 		label: qsTr("Fisher transformed") 			},
						{ value: "arithmeticMean", 			label: qsTr("Arithmetic mean")				},
						{ value: "geometricMean", 			label: qsTr("Geometric mean")				},
						{ value: "harmonicMean", 			label: qsTr("Harmonic mean")				},
						{ value: "geometricHarmonicMean", 	label: qsTr("Geometric-harmonic mean")		}
					]
				}
			}

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
			RadioButtonGroup
			{
				title: qsTr("Error calculation method")
				name: "errorCalculationMethod"
				RadioButton { value: "none";		label: qsTr("None"); checked: true	}
				RadioButton { value: "robust";	label: qsTr("Robust")				}
				RadioButton
				{
					value: "bootstrap";	label: qsTr("Bootstrap")
					IntegerField
					{
						name: "bootstrapSamples"
						label: qsTr("Bootstrap samples")
						fieldWidth: 60
						defaultValue: 200
						min: 1
					}
				}
			}
			CIField {
				text: qsTr("Confidence intervals")
				name: "ciLevel"
			}
			SetSeed {}
		}

		Group
		{
			DropDown
			{
				name: "weightingApproach"
				label: qsTr("Weighting approach")
				id: approachWeigths 
				values: 
				[
					{ label: qsTr("PLS-PM"), 		value: "PLS-PM" 		},
					{ label: qsTr("GSCA"), 			value: "GSCA"			},
					{ label: qsTr("SUMCORR"), 		value: "SUMCORR"	 	},
					{ label: qsTr("MAXVAR"), 		value: "MAXVAR" 		},
					{ label: qsTr("SSQCORR"), 		value: "SSQCORR" 		},
					{ label: qsTr("MINVAR"), 		value: "MINVAR" 		},
					{ label: qsTr("GENVAR"), 		value: "GENVAR" 		},
					{ label: qsTr("PCA"), 			value: "PCA"			},
					{ label: qsTr("Unit"), 			value: "unit"			},
					{ label: qsTr("Bartlett"), 		value: "bartlett"		},
					{ label: qsTr("Regression"), 	value: "regression"		}
				] 
			}
			
			DropDown
			{
				enabled: approachWeigths.currentValue == "PLS-PM"
				name: "innerWeightingScheme"
				label: qsTr("Inner weighting scheme")
				values: [
					{ value: "path", 		label: qsTr("Path")			},
					{ value: "centroid", 	label: qsTr("Centroid")		},
					{ value: "factorial", 	label: qsTr("Factorial")	}
				]
			}

			DropDown
			{
				name: "convergenceCriterion"
				label: qsTr("Convergence criterion")
				values: [
					{ value: "absoluteDifference",	label: qsTr("Absolute difference")	},
					{ value: "squaredDifference",	label: qsTr("Squared difference")	},
					{ value: "relativeDifference",	label: qsTr("Relative difference")	}
				]
			}

			RadioButtonGroup
			{
				title: qsTr("Correlation matrix")
				name: "correlationMatrix"
				RadioButton { value: "pearson"	; label: qsTr("Pearson"); checked: true	}
				RadioButton { value: "spearman" ; label: qsTr("Spearman")				}
			}

			RadioButtonGroup
			{
				title: qsTr("Handling of inadmissibles")
				name: "handlingOfInadmissibles"
				RadioButton { value: "replace"; label: qsTr("Replace")	; checked: true	}
				RadioButton { value: "ignore"; 	label: qsTr("Ignore")					}
				RadioButton { value: "drop"; 	label: qsTr("Drop")						}
			}

			DropDown
				{
					name: "handlingOfFlippedSigns"
					label: qsTr("Handling of flipped signs")
					values: [
						{ value: "none", 					label: qsTr("None")						},
						{ value: "individual", 				label: qsTr("Individual")				},
						{ value: "individualReestimation", 	label: qsTr("Individual re-estimation")	},
						{ value: "constructReestimation", 	label: qsTr("Construct re-estimation") 	}
					]
				}
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
	}

	Section
	{
		title: "Prediction"

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
				RadioButton { value: "PLS-PM"; 	label: qsTr("PLS-PM")			}
				RadioButton { value: "GSCA"; 	label: qsTr("GSCA")				}
				RadioButton { value: "PCA";		label: qsTr("PCA")				}
				RadioButton { value: "MAXVAR";	label: qsTr("MAXVAR")			}
				RadioButton { value: "all";		label: qsTr("All")					}
			}

			CheckBox { name: "predictedScore";	label: qsTr("Show predicted scores"); enabled: prediction.checked}
		}
	}
}
