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
		optionKey: "modelName"

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeCSem }
	}

	Section
	{
		title: qsTr("Model")


		Group
		{
			DropDown
			{
				name: "approachWeights"
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
			CheckBox
			{
				enabled: approachWeigths.currentValue == "PLS-PM"
				name: "ignoreStructuralModel"
				label: qsTr("Ignore structural model")
			}

			DropDown
			{
				name: "convergenceCriterion"
				label: qsTr("Convergence criterion")
				values: [
					{ value: "diff_absolute",	label: qsTr("Absolute difference")	},
					{ value: "diff_squared",	label: qsTr("Squared difference")	},
					{ value: "diff_relative",	label: qsTr("Relative difference")	}
				]
			}
		}

		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Correlation matrix")
				name: "approachCorRobust"
				RadioButton { value: "none"		; label: qsTr("Pearson"); checked: true	}
				RadioButton { value: "spearman" ; label: qsTr("Spearman")				}
			}

			CheckBox
			{
				name: "disattenuate";		label: qsTr("Disattenuate composite correlations");	checked: true
				DropDown
				{
					name: "approachCorrectionFactors"
					label: qsTr("Approach correction factors")
					values: [
						{ value: "dist_squared_euclid", 	label: qsTr("Squared Euclidean distance")	},
						{ value: "dist_euclid_weighted", 	label: qsTr("Weighted Euclidean distance")	},
						{ value: "fisher_transformed", 		label: qsTr("Fisher transformed") 			},
						{ value: "mean_arithmetic", 		label: qsTr("Arithmetic mean")				},
						{ value: "mean_geometric", 			label: qsTr("Geometric mean")				},
						{ value: "mean_harmonic", 			label: qsTr("Harmonic mean")				},
						{ value: "geo_of_harmonic", 		label: qsTr("Geometric-harmonic mean")		}
					]
				}
			}

			DropDown
			{
				id: grpvar
				name: "groupingVariable"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
			}
		}
	}

	Section
	{
		title: qsTr("Resampling")

		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Error calculation method")
				name: "resamplingMethod"
				RadioButton { value: "none";		label: qsTr("None"); checked: true	}
				RadioButton { value: "jackknife";	label: qsTr("Robust")				}
				RadioButton
				{
					value: "bootstrap";	label: qsTr("Bootstrap")
					IntegerField
					{
						name: "nBootstraps"
						label: qsTr("Bootstrap samples")
						fieldWidth: 60
						defaultValue: 200
						min: 1
					}
				}
			}
			CIField {
				text: qsTr("Confidence intervals")
				name: "ciWidth"
			}
			SetSeed {}
		}

		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Inadmissibles handling")
				name: "handleInadmissibles"
				RadioButton { value: "replace"; label: qsTr("Replace")	; checked: true	}
				RadioButton { value: "ignore"; 	label: qsTr("Ignore")					}
				RadioButton { value: "drop"; 	label: qsTr("Drop")						}
			}

			DropDown
				{
					name: "signFlippingHandling"
					label: qsTr("Sign flipping handling")
					values: [
						{ value: "none", 					label: qsTr("None")						},
						{ value: "individual", 				label: qsTr("Indiviual")				},
						{ value: "individual_reestimate", 	label: qsTr("Individual re-estimate")	},
						{ value: "construct_reestimate", 	label: qsTr("Construct re-estimate") 	}
					]
				}
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

	Section
	{
		title: qsTr("Output")

		Group
		{
		  	CheckBox { name: "outputRSquared";				label: qsTr("R-squared")				}
			CheckBox { name: "outputAdditionalFitMeasures";	label: qsTr("Additional fit measures")	}
			CheckBox { name: "outputMardiasCoefficients";	label: qsTr("Mardia's coefficient")		}
			CheckBox { name: "outputReliabilityMeasures";	label: qsTr("Reliability measures")		}
		}

		Group
		{
		  	CheckBox { name: "outputObservedIndicatorCorrelations";	label: qsTr("Observed indicator correlations")	}
			CheckBox { name: "outputImpliedIndicatorCorrelations";	label: qsTr("Implied indicator correlations")	}
			CheckBox { name: "outputObservedConstructCorrelations"; label: qsTr("Observed construct correlations")	}
			CheckBox { name: "outputImpliedConstructCorrelations"; 	label: qsTr("Implied construct correlations")	}
		}
	}
}
