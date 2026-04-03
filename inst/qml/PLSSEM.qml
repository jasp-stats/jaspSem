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
		info: qsTr("Specify the PLS-SEM model using cSEM syntax. Define measurement models (e.g., 'eta1 =~ y1 + y2 + y3') and structural model (e.g., 'eta2 ~ eta1').")

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeCSem; showLineNumber: true }
	}

	Section
	{
		title: qsTr("Model")
		info: qsTr("Model-level settings such as grouping variables for multi-group analysis.")

		Group
		{

			DropDown
			{
				id: grpvar
				name: "group"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
				allowedColumns: ["nominal"]
				info: qsTr("Select a grouping variable to perform multi-group PLS-SEM analysis.")
			}
		}
	}

	Section
	{
		title: qsTr("Estimation")
		info: qsTr("Settings for the PLS-SEM estimation algorithm, convergence criteria, and error calculation.")

		Group 
		{
			CheckBox
			{
				name: "consistentPartialLeastSquares";		label: qsTr("Consistent partial least squares");	checked: true
				info: qsTr("Use consistent PLS (PLSc) to correct for attenuation, producing consistent estimates for common factor models.")
			}
			DropDown
			{
				name: "innerWeightingScheme"
				label: qsTr("Inner weighting scheme")
				id: approachInner
				info: qsTr("Choose the scheme for estimating inner weights relating constructs to each other.")
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
				info: qsTr("Estimate the measurement model only, ignoring the structural (inner) model.")
			}

			DropDown
			{
				name: "convergenceCriterion"
				label: qsTr("Convergence criterion")
				info: qsTr("Criterion used to assess convergence of the PLS algorithm.")
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
				info: qsTr("Tolerance threshold for the convergence criterion. Smaller values require stricter convergence.")
			}

		}
		
		Group
		{
			title: qsTr("Error calculation method")
			info: qsTr("Method for computing standard errors and confidence intervals.")
			RadioButtonGroup
			{
				name: "errorCalculationMethod"
				id: errorCalcMethod
				info: qsTr("Select the error calculation method.")
				RadioButton { value: "none";		label: qsTr("None"); checked: true;	info: qsTr("Do not compute standard errors or confidence intervals.") }
				RadioButton {
							value: "bootstrap";	label: qsTr("Bootstrap"); checked: true
							info: qsTr("Compute standard errors and confidence intervals using bootstrap resampling.")
							IntegerField
							{
								name: "bootstrapSamples"
								label: qsTr("Samples")
								fieldWidth: 60
								defaultValue: 200
								min: 1
								info: qsTr("Number of bootstrap resamples to draw.")
								// enabled: errorCalcMethod.value == "robust"
							}
							CIField
							{
								text: qsTr("Confidence intervals")
								name: "ciLevel"
								enabled: errorCalcMethod.value == "bootstrap"
								info: qsTr("Width of the bootstrap confidence intervals.")
							}
						}
				}
			RadioButtonGroup
			{
				visible: errorCalcMethod.value != "none"
				title: qsTr("Handling of inadmissibles")
				name: "handlingOfInadmissibles"
				info: qsTr("How to handle bootstrap samples that produce inadmissible results (e.g., Heywood cases).")
				RadioButton { value: "replace"; label: qsTr("Replace")	; checked: true;	info: qsTr("Replace inadmissible bootstrap samples with new ones.") }
				RadioButton { value: "ignore"; 	label: qsTr("Ignore");						info: qsTr("Keep inadmissible results in the bootstrap distribution.") }
				RadioButton { value: "drop"; 	label: qsTr("Drop");						info: qsTr("Drop inadmissible bootstrap samples, reducing the effective number of resamples.") }
			}
		}

		SetSeed {}

	}

	Section
	{
		title: qsTr("Output")
		info: qsTr("Additional output tables and statistics to display.")

		Group
		{
		  CheckBox { name: "rSquared";							label: qsTr("R-squared");				info: qsTr("Display the proportion of variance explained for each endogenous construct.") }
			CheckBox { name: "additionalFitMeasures";	label: qsTr("Additional fit measures");	info: qsTr("Display additional fit indices for evaluating model fit.") }
			CheckBox { name: "mardiasCoefficient";		label: qsTr("Mardia's coefficient");	info: qsTr("Display Mardia's multivariate skewness and kurtosis coefficients to assess multivariate normality.") }
			CheckBox { name: "reliabilityMeasures";		label: qsTr("Reliability measures");	info: qsTr("Display reliability measures for each construct (e.g., Cronbach's alpha, composite reliability).") }
		}

		Group
		{
		  CheckBox { name: "observedIndicatorCorrelation";	label: qsTr("Observed indicator correlations");	info: qsTr("Display the observed correlation matrix of the indicator variables.") }
			CheckBox { name: "impliedIndicatorCorrelation";		label: qsTr("Implied indicator correlations");	info: qsTr("Display the model-implied correlation matrix of the indicator variables.") }
			CheckBox { name: "observedConstructCorrelation"; 	label: qsTr("Observed construct correlations");	info: qsTr("Display the observed correlation matrix of the constructs.") }
			CheckBox { name: "impliedConstructCorrelation"; 	label: qsTr("Implied construct correlations");	info: qsTr("Display the model-implied correlation matrix of the constructs.") }
		}

		Group 
		{
			CheckBox { name: "overallModelFit"; label: qsTr("Overall model fit") ; id: omf;	info: qsTr("Perform an overall model fit test using bootstrap-based tests.") }
			IntegerField { visible:omf.checked; name: "omfBootstrapSamples"; label: qsTr("Bootstrap samples"); fieldWidth: 60; defaultValue: 499; min: 100; info: qsTr("Number of bootstrap samples for the overall model fit test.") }
			CIField { visible: omf.checked; text: qsTr("Significance level"); name: "omfSignificanceLevel"; defaultValue: 5; info: qsTr("Significance level for the overall model fit test.") }
			CheckBox { visible: omf.checked; name: "saturatedStructuralModel"; label: qsTr("Saturated structural model"); info: qsTr("Use a saturated structural model as a reference for the model fit test.") }
		}

		CheckBox
		{
			name: "addConstructScores"
			text: qsTr("Add construct scores to data")
			info: qsTr("Save estimated construct scores as new columns in the dataset.")
		}
	}

	Section
	{
		title: qsTr("Prediction")
		info: qsTr("Settings for cross-validated prediction of endogenous indicator scores.")

		Group
		{
			CheckBox
			{	name: "endogenousIndicatorPrediction"
				label: qsTr("Predict endogenous indicator scores")
				id: prediction
				info: qsTr("Perform k-fold cross-validation to predict endogenous indicator scores.")
			}

			IntegerField
			{
				name: "kFolds"
				label: qsTr("Cross-validation k-folds")
				fieldWidth: 60
				defaultValue: 10
				min: 2
				enabled: prediction.checked
				info: qsTr("Number of folds for cross-validation. Higher values increase computation time but reduce bias.")
			}

			IntegerField
			{
				name: "repetitions"
				label: qsTr("Repetitions")
				fieldWidth: 60
				defaultValue: 10
				min: 1
				enabled: prediction.checked
				info: qsTr("Number of times the cross-validation is repeated to reduce variance in the prediction metrics.")
			}

			RadioButtonGroup
			{
				title: qsTr("Benchmark(s)")
				name: "benchmark"
				enabled: prediction.checked
				info: qsTr("Select a benchmark model to compare the prediction accuracy of the PLS-SEM model.")
				RadioButton { value: "none"; 	label: qsTr("None")	; checked: true;	info: qsTr("No benchmark comparison.") }
				RadioButton { value: "lm"; 		label: qsTr("Linear model");			info: qsTr("Compare against a linear regression model.") }
				RadioButton { value: "GSCA"; 	label: qsTr("GSCA");					info: qsTr("Compare against Generalized Structured Component Analysis.") }
				RadioButton { value: "PCA";		label: qsTr("PCA");						info: qsTr("Compare against Principal Component Analysis.") }
				RadioButton { value: "MAXVAR";	label: qsTr("MAXVAR");					info: qsTr("Compare against the MAXVAR approach.") }
				RadioButton { value: "all";		label: qsTr("All");						info: qsTr("Compare against all available benchmark models.") }
			}
		}
	}
}
