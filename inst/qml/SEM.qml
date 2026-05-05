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

Form
{

	columns: 1

	// The following part is used for spawning upgrade notifications about multigroup analysis
	Rectangle
	{
		visible:		needsRefresh && grpvar.currentIndex !== 0 // if groupvar index is 0, there is no grouping variable -> no multigroup analysis
		color:			jaspTheme.controlWarningBackgroundColor
		width:			form.implicitWidth
		height:			warningMessageUpdate.height
		radius:			jaspTheme.borderRadius

		Text
		{
			id:					warningMessageUpdate
			text:				qsTr("This analysis was created with an older version of JASP (or a dynamic module). Since then, there were changes in the lavaan package that runs the analysis. Specifically, the lavaan syntax for equality constraints in multigroup analysis is now interpreted differently. Proceed with caution! More details about the update can be found at %1").arg("https://groups.google.com/g/lavaan/c/HSavF8oaW5M")
			color:				jaspTheme.controlWarningTextColor
			anchors.top:		parent.top
			padding:			5 * jaspTheme.uiScale
			wrapMode:			Text.Wrap
			width:				parent.width - 10 * jaspTheme.uiScale
			verticalAlignment:	Text.AlignVCenter
		}
	}
	// end upgrade notifications

	TabView
	{
		id: models
		name: "models"
		maximumItems: 9
		newItemName: qsTr("Model 1")
		optionKey: "name"
		info: qsTr("Specify the structural equation model using lavaan syntax. Multiple models can be specified and compared. See lavaan.org for syntax tutorials.")

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan; showLineNumber: true; info: qsTr("Enter the lavaan model syntax using variable names from the dataset.") }
	}

	RadioButtonGroup
	{
		title: qsTr("Data")
		name: "dataType"
		columns: 2
		info: qsTr("Select whether the input is a raw data matrix or a variance-covariance matrix.")
		RadioButton { value: "raw"; label: qsTr("Raw"); checked: true; info: qsTr("Use raw data with observations as rows and variables in columns.") }
		RadioButton
		{
			value: "varianceCovariance"; label: qsTr("Variance-covariance matrix")
			info: qsTr("Use a variance-covariance matrix as input. Requires specifying the sample size.")
			IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 500; info: qsTr("The number of observations the covariance matrix is based on.") }
		}
	}

	DropDown
	{
		name: "samplingWeights"
		label: qsTr("Sampling weights")
		showVariableTypeIcon: true
		addEmptyValue: true
		info: qsTr("Select a variable from the dataset to use as sampling weights for each observation.")
	}

	Section
	{
		title: qsTr("Model Options")
		info: qsTr("Options for configuring the structural equation model specification.")
		Group
		{
			DropDown
			{
				name: "factorScaling"
				label: qsTr("Factor scaling")
				info: qsTr("How the metric of latent variables is determined: by fixing the first loading to 1 (Factor loadings), fixing factor variance to 1 (Factor variance), fixing average loadings to 1 (Effects coding), or none.")
				values:
				[
					{ label: qsTr("Factor loadings"),	value: "factorLoading"	},
					{ label: qsTr("Factor variance"),	value: "factorVariance"	},
					{ label: qsTr("Effects coding"),	value: "effectCoding"	},
					{ label: qsTr("None"),				value: "none"			}
				]
			}
			CheckBox 
			{
				name: "meanStructure"
				label: qsTr("Mean structure")
				checked: eq_intercepts.checked || eq_means.checked || eq_thresholds.checked
				info: qsTr("Estimate observed intercepts and/or latent means. Some elements need to be fixed for identification.")
				CheckBox { name: "latentInterceptFixedToZero";		label: qsTr("Latent means fixed to zero");					checked: !eq_means.checked;	info: qsTr("Fix latent means to zero; observed intercepts are estimated freely.")  }
				CheckBox { name: "manifestInterceptFixedToZero";	label: qsTr("Manifest intercepts fixed to zero");			info: qsTr("Fix observed intercepts to zero; latent means are estimated freely.") }
				CheckBox { name: "manifestMeanFixedToZero";			label: qsTr("Mean of manifest intercepts fixed to zero");	info: qsTr("Fix the mean of manifest intercepts to zero; latent means are estimated freely.") }
			}
			
			CheckBox { name: "orthogonal";	label: qsTr("Assume factors uncorrelated"); info: qsTr("Estimate factors as orthogonal (uncorrelated) instead of allowing them to correlate.") }
		}

		Group
		{
			CheckBox 
			{
				name:		"exogenousCovariateFixed"
				label:		qsTr("Exogenous covariate(s) fixed")
				id:			fix_x
				checked:	true
				info:		qsTr("If checked, exogenous covariates are considered fixed and their means, variances, and covariances are fixed to sample values.")
			}
			CheckBox 
			{
				name:		"exogenousCovariateConditional"
				label:		qsTr("Condition on exogenous covariate(s)")
				id:			cond_x
				// Try to mimic lavaan's default setting for conditional.x
				checked:	["wls", "dwls", "wlsm", "wlsmv"].includes(estimator.value) && errorCalc.value != "bootstrap"
				info:		qsTr("Condition on the exogenous covariates when estimating the model.")
			}
			CheckBox { name: "residualSingleIndicatorOmitted";	label: qsTr("Residual single indicator omitted");	checked: true;	info: qsTr("Set the residual variance of a single indicator to zero if it is the only indicator of a latent variable.") }
			CheckBox { name: "residualVariance";				label: qsTr("Residual variances included");			checked: true;	info: qsTr("Include residual variances of observed and latent variables as free parameters.") }
			CheckBox { name: "exogenousLatentCorrelation";		label: qsTr("Exogenous latents correlated");		checked: true;	info: qsTr("Include covariances of exogenous latent variables in the model.") }
			CheckBox { name: "dependentCorrelation";			label: qsTr("Dependent variables correlated");		checked: true;	info: qsTr("Include covariances of dependent variables (observed and latent) in the model.") }
			CheckBox { name: "threshold";						label: qsTr("Thresholds");							checked: true;	info: qsTr("Include thresholds for limited (non-continuous) dependent variables.") }
			CheckBox { name: "scalingParameter";				label: qsTr("Scaling parameters");					checked: true;	info: qsTr("Include response scaling parameters for limited (non-continuous) dependent variables.") }
			CheckBox { name: "efaConstrained";					label: qsTr("EFA blocks constrained");				checked: true;	info: qsTr("Impose constraints to make exploratory factor analysis blocks identifiable: factor variances set to 1, covariances to zero, and loadings follow an echelon pattern.") }
		}
	}

	Section
	{
		title: qsTr("Estimation Options")
		info: qsTr("Options for the estimation method, model test, standard errors, confidence intervals, and missing data handling.")

		Group
		{
			RowLayout 
			{
				DropDown
				{
					name: "estimator"
					label: qsTr("Estimator")
					id: estimator
					info: qsTr("Choose the estimation method. Some estimators set implicit options for test and standard errors. ML-based extensions: MLM (robust SE, Satorra-Bentler test), MLMV (robust SE, scaled-shifted test), MLMVS (robust SE, Satterthwaite test), MLF (first-order SE), MLR (Huber-White SE, Yuan-Bentler test). WLS variants: WLSM/WLSMV imply DWLS with robust SE, ULSM/ULSMV imply ULS with robust SE.")
					values: [
						{ value: "default",	label: qsTr("Default")	},
						{ value: "ml",		label: qsTr("ML")		},
						{ value: "gls",		label: qsTr("GLS")		},
						{ value: "wls",		label: qsTr("WLS")		},
						{ value: "uls",		label: qsTr("ULS")		},
						{ value: "dwls",	label: qsTr("DWLS")		},
						{ value: "dls",		label: qsTr("DLS")		},
						{ value: "pml",		label: qsTr("PML")		},
						{ value: "mlm",		label: qsTr("MLM")		},
						{ value: "mlmv",	label: qsTr("MLMV")		},
						{ value: "mlmvs",	label: qsTr("MLMVS")	},
						{ value: "mlf",		label: qsTr("MLF")		},
						{ value: "mlr",		label: qsTr("MLR")		},
						{ value: "wlsm",	label: qsTr("WLSM")		},
						{ value: "wlsmv",	label: qsTr("WLSMV")	},
						{ value: "ulsm",	label: qsTr("ULSM")		},
						{ value: "ulsmv",	label: qsTr("ULSMV")	}
					]
				}
			}
			
			DropDown
			{
				name: "modelTest"
				id: modTest
				label: qsTr("Model test")
				info: qsTr("Choose the test statistic for evaluating model fit. If left at Default, the test is determined by the chosen estimator.")
				values: [
					{ value: "default", 				label: qsTr("Default")						},
					{ value: "standard",				label: qsTr("Standard")						},
					{ value: "satorraBentler",			label: qsTr("Satorra-Bentler")				},
					{ value: "yuanBentler",				label: qsTr("Yuan-Bentler")					},
					{ value: "yuanBentlerMplus",		label: qsTr("Yuan-Bentler Mplus")			},
					{ value: "meanAndVarianceAdjusted",	label: qsTr("Mean and variance adjusted")	},
					{ value: "scaledAndShifted",		label: qsTr("Scaled and shifted")			},
					{ value: "bollenStine",				label: qsTr("Bootstrap (Bollen-Stine)")		}, 
					{ value: "browneResidualAdf", 		label: qsTr("Browne residual based (ADF)")	}, 
					{ value: "browneResidualNt", 		label: qsTr("Browne residual based (NT)")	}
				]
			}

			IntegerField
			{
				visible: modTest.value == "bollenStine"
				name: "bootstrapSamplesBollenStine"
				label: qsTr("     Bootstrap samples")
				fieldWidth: 60
				defaultValue: 1000
				min: 1
				info: qsTr("Number of bootstrap samples for the Bollen-Stine bootstrap test.")
			}

			DropDown
			{
				label: qsTr("Information matrix")
				name: "informationMatrix"
				info: qsTr("Matrix used to compute the standard errors: expected, observed, or first order (outer product of casewise scores).")
				values: [
					{ value: "default",		label: qsTr("Default")		},
					{ value: "expected",	label: qsTr("Expected") 	},
					{ value: "observed",	label: qsTr("Observed") 	},
					{ value: "firstOrder",	label: qsTr("First order")	}
				]
			}

			DropDown
			{
				label: qsTr("Standard errors")
				name: "errorCalculationMethod"
				id: errorCalc
				info: qsTr("Method for computing standard errors. Standard uses the information matrix, Robust uses robust.sem, Robust Huber-White uses the mlr approach, and Bootstrap computes SEs from bootstrapped fits.")
				values: [
					{ value: "default", 	 		label: qsTr("Default")				},
					{ value: "standard",  	 		label: qsTr("Standard") 			},
					{ value: "robust", 	 			label: qsTr("Robust") 				},
					{ value: "robustHuberWhite", 	label: qsTr("Robust Huber-White")	},
					{ value: "bootstrap", 			label: qsTr("Bootstrap")			}
				]
			}

			IntegerField
			{
				visible: errorCalc.value == "bootstrap"
				name: "bootstrapSamples"
				label: qsTr("     Bootstrap samples")
				fieldWidth: 60
				defaultValue: 1000
				min: 1
				info: qsTr("Number of bootstrap samples to use for computing standard errors.")
			}

			DropDown {
				visible: errorCalc.value == "bootstrap"
				label: qsTr("     Type")
				name: "bootstrapCiType"
				info: qsTr("Type of bootstrap confidence interval.")
				values: [
					{ label: qsTr("Bias-corrected percentile"),	value: "percentileBiasCorrected"	},
					{ label: qsTr("Percentile"),				value: "percentile"					},
					{ label: qsTr("Normal theory"),				value: "normalTheory"				}
				]
			}

			CIField {
				text: qsTr("Confidence intervals")
				name: "ciLevel"
				info: qsTr("Width of the confidence intervals for parameter estimates.")
			}

			CheckBox
			{
				visible:	errorCalc.value == "bootstrap" || modTest.value == "bollenStine"
				name:		"userGaveSeed"
				id:			user_seed
				label:		qsTr("Set a random seed:")
				checked:	false
				childrenOnSameRow: true
				info:		qsTr("Set a seed to guarantee reproducible bootstrap results.")

				IntegerField
				{
					name:			"bootSeed"
					defaultValue:	1
					min:			1
				}
			}


		}

		Group
		{
			id: missingG
			CheckBox{name: "standardizedVariable"; label: qsTr("Standardize variables before estimation"); checked: false; info: qsTr("Z-standardize all variables before estimation.")}

			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				info: qsTr("How to treat missing values. FIML uses full-information maximum likelihood. Pairwise computes correlations using available pairs. Two-stage uses EM-estimated statistics. Robust two-stage adds robustness against non-normality. Doubly robust is for PML estimation.")
				values: [
					{ label: qsTr("(FI)ML"), 						value: "fiml"						},
					{ label: qsTr("Listwise deletion"), value: "listwise"				},
					{ label: qsTr("Pairwise"), 					value: "pairwise"				},
					{ label: qsTr("Two-stage"), 				value: "twoStage"				},
					{ label: qsTr("Robust two-stage"), 	value: "twoStageRobust"	},
					{ label: qsTr("Doubly robust"), 		value: "doublyRobust"		}
				]

			}

			DropDown
			{
				name: "emulation"
				label: qsTr("Mimic")
				info: qsTr("Emulate the output from different SEM programs.")
				values: [
					{ value: "lavaan",	label: qsTr("Lavaan") 	},
					{ value: "mplus",	label: qsTr("Mplus") 	},
					{ value: "eqs",		label: qsTr("EQS") 		}
				]
			}

		}
	}

	Section
	{
		title: qsTr("Output Options")
		info: qsTr("Options for additional fit measures, parameter estimates, covariance matrices, plots, and modification indices.")

		Group
		{
			CheckBox { name: "additionalFitMeasures";	label: qsTr("Additional fit measures");	info: qsTr("Display a table with various fit measures including CFI, TLI, RMSEA, SRMR, and information criteria.") }
			CheckBox { name: "rSquared";				label: qsTr("R-squared");							info: qsTr("Display the explained variance in each dependent variable.") }
			CheckBox { name: "ave";						label: qsTr("Average variance extracted (AVE)");	info: qsTr("Display the amount of variance captured by a construct relative to measurement error. Used to evaluate convergent validity.") }
			CheckBox { name: "htmt";					label: qsTr("Heterotrait-monotrait ratio (HTMT)");	info: qsTr("Display the HTMT ratio to assess discriminant validity between constructs.") }
			CheckBox { name: "reliability";				label: qsTr("Reliability measures");				info: qsTr("Display reliability metrics such as coefficient alpha and composite (omega) reliability per latent variable.") }
			CheckBox { name: "mardiasCoefficient";		label: qsTr("Mardia's coefficient");				info: qsTr("Display Mardia's multivariate skewness and kurtosis coefficients to assess multivariate normality.") }
			CheckBox { name: "observedCovariance";		label: qsTr("Observed covariances");				info: qsTr("Display the observed covariance matrix calculated from the data.") }
			CheckBox { name: "impliedCovariance";		label: qsTr("Implied covariances");				info: qsTr("Display the model-implied covariance matrix based on estimated parameters.") }
			CheckBox { name: "residualCovariance";		label: qsTr("Residual covariances");				info: qsTr("Display residual covariances (observed minus implied).") }
			CheckBox { name: "standardizedResidual"; 	label: qsTr("Standardized residuals");			info: qsTr("Display the standardized residual covariance matrix.") }
		}

		Group
		{
			CheckBox
			{
				name: "standardizedEstimate"; label: qsTr("Standardized estimates");
				info: qsTr("Display standardized parameter estimates.")
				RadioButtonGroup
				{
					name: "standardizedEstimateType"
					info: qsTr("Type of standardization.")
					RadioButton { value: "all"; 	label: qsTr("All"); checked: true;	info: qsTr("Standardize based on variances of both observed and latent variables.") }
					RadioButton { value: "latents"; label: qsTr("Latents");				info: qsTr("Standardize based on latent variable variances only.") }
					RadioButton { value: "nox"; 	label: qsTr("Except exogenous covariates");	info: qsTr("Standardize based on variances of observed and latent variables, excluding exogenous covariates.") }
				}
			}
			CheckBox
			{
				name: "pathPlot";
				text: qsTr("Path diagram");
				checked: false
				info: qsTr("Display a path diagram of the model.")
				CheckBox {
					name: "pathPlotParameter"
					text: qsTr("Show parameter estimates")
					checked: false
					info: qsTr("Display parameter estimates on the path diagram.")
					CheckBox {
						name: "pathPlotParameterStandardized"
						text: qsTr("Standardized")
						checked: false
						info: qsTr("Show standardized estimates on the path diagram.")
					}
				}
				CheckBox {
					name: "pathPlotLegend"
					text: qsTr("Show legend")
					checked: false
					info: qsTr("Display a legend in the path diagram.")
				}
			}
			CheckBox
			{
				name: "modificationIndex"
				label: qsTr("Modification indices")
				info: qsTr("Display modification indices showing potential improvements to the model.")
				CheckBox
				{
					name: "modificationIndexHiddenLow"
					label: qsTr("Hide low indices")
					info: qsTr("Hide modification indices below the specified threshold.")
					DoubleField
					{
						name: "modificationIndexThreshold"
						label: qsTr("Threshold")
						negativeValues: false
						decimals: 2
						defaultValue: 10
						info: qsTr("Minimum value for modification indices to be displayed.")
					}
				}
			}
			CheckBox 
			{ 
				name: "warnings";	
				label: qsTr("Show warnings")
				info: qsTr("Display warnings produced by lavaan during estimation.")
			}

		}
	}

	Section
	{
		title: qsTr("Multigroup SEM")
		info: qsTr("Options for fitting the model across multiple groups, with equality constraints on parameters.")

		Group
		{
			DropDown
			{
				id: grpvar
				name: "group"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
				info: qsTr("Select a nominal or ordinal variable to fit the model separately for each group.")
			}
			Group
			{
				visible: grpvar.value != ""
				id: constraints
				title: qsTr("Equality Constraints")
				info: qsTr("Constrain selected parameters to be equal across all groups.")
				CheckBox { id: eq_loadings; 			name: "equalLoading";				label: qsTr("Loadings");			info: qsTr("Constrain factor loadings to be equal across groups.") }
				CheckBox { id: eq_intercepts; 			name: "equalIntercept";				label: qsTr("Intercepts");			info: qsTr("Constrain intercepts to be equal across groups.") }
				CheckBox { id: eq_residuals; 			name: "equalResidual";				label: qsTr("Residuals");			info: qsTr("Constrain residual variances to be equal across groups.") }
				CheckBox { id: eq_residualcovariances; 	name: "equalResidualCovariance";	label: qsTr("Residual covariances");	info: qsTr("Constrain residual covariances to be equal across groups.") }
				CheckBox { id: eq_means; 				name: "equalMean";					label: qsTr("Means");				info: qsTr("Constrain means to be equal across groups.") }
				CheckBox { id: eq_thresholds; 			name: "equalThreshold";				label: qsTr("Thresholds");			info: qsTr("Constrain thresholds to be equal across groups.") }
				CheckBox { id: eq_regressions; 			name: "equalRegression";			label: qsTr("Regressions");			info: qsTr("Constrain regression coefficients to be equal across groups.") }
				CheckBox { id: eq_variances; 			name: "equalLatentVariance";		label: qsTr("Latent variances");	info: qsTr("Constrain latent variable variances to be equal across groups.") }
				CheckBox { id: eq_lvcovariances; 		name: "equalLatentCovariance";		label: qsTr("Latent covariances");	info: qsTr("Constrain latent variable covariances to be equal across groups.") }
			} // No model or source: it takes all variables per default
		}	

		TextArea
		{
			name: "freeParameters"
			title: qsTr("Release constraints (one per line)")
			width: 250
			height: constraints.height + grpvar.height
			textType: JASP.TextTypeLavaan
			visible: eq_loadings.checked || eq_intercepts.checked || eq_residuals.checked || eq_residualcovariances.checked || eq_means.checked || eq_thresholds.checked || eq_regressions.checked || eq_variances.checked || eq_lvcovariances.checked
			info: qsTr("Release specific equality constraints using lavaan syntax, e.g., 'f=~x2' to release a single loading.")
		}
		
	}


	Section 
	{
		title: qsTr("Sensitivity Analysis")
		info: qsTr("Conduct sensitivity analysis against a potential missing confounder using phantom variables.")
		CheckBox
		{
			name: "sensitivityAnalysis"
			label: qsTr("Run sensitivity analysis")
			info: qsTr("Run a sensitivity analysis by adding phantom variables (latent variables without indicators) to assess the robustness of the model.")
			RadioButtonGroup
			{
				title: qsTr("Search algorithm")
				name: "searchAlgorithm"
				id: search
				info: qsTr("Algorithm for sampling phantom variables.")
				RadioButton
				{
					value: "antColonyOptimization"
					label: qsTr("Ant colony optimization")
					checked: true
					info: qsTr("Use ant colony optimization to find good paths through the parameter space.")
					IntegerField { name: "numberOfAnts"; 			label: qsTr("Number of ants"); 					defaultValue: 10;	info: qsTr("Number of artificial ants per iteration, each representing a potential solution.") }
					IntegerField { name: "sizeOfSolutionArchive"; 	label: qsTr("Size of the solution archive"); 	defaultValue: 100;	info: qsTr("Number of best solutions stored during optimization.") }
					DoubleField { name: "convergenceRateThreshold";	label: qsTr("Convergence rate threshold");		defaultValue: 0.1;	negativeValues: false;	info: qsTr("If the change in the objective function is smaller than this threshold, the algorithm has converged.") }
				}
				RadioButton { value: "tabuSearch"; 				label: qsTr("Tabu search");	info: qsTr("Use tabu search, which maintains a list of recently visited solutions to avoid local optima.") }
			}
			DropDown
			{
				name: "optimizerFunction"
				label: qsTr("Optimizer function")
				info: qsTr("Objective function maximized during the optimization.")
				values:
				[
					{ label: qsTr("% change mean estimate")			, value: "percentChangeMeanEstimate"	},
					{ label: qsTr("SD of deviance / old estimate")	, value: "sdOfDeviance"					},
					{ label: qsTr("Change of p-value")				, value: "changeOfPvalue"				},
					{ label: qsTr("Distance of p-value from alpha")	, value: "distanceOfPvalue"				},
					{ label: qsTr("Change of RMSEA")				, value: "changeOfRmsea"				},
					{ label: qsTr("Distance of RMSEA from 0.05")	, value: "distanceOfRmsea"				}
				]
			}
			DoubleField
			{
				name: "alpha"
				label: qsTr("Significance level")
				negativeValues: false
				decimals: 4
				defaultValue: 0.05
				info: qsTr("Significance level for the sensitivity analysis.")
			}
			IntegerField { name: "maxIterations"; 	label: qsTr("Maximum number of iterations"); 	defaultValue: 1000;	info: qsTr("Maximum number of iterations for the optimization algorithm.") }
			SetSeed{}
		}
	}
}
