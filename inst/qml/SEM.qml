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
			text:				qsTr("This analysis was created with an older version of JASP (or a dynamic module). Since then, there were changes in the lavaan package that runs the analysis. Specifically, the lavaan syntax for equality constraints in multigroup analysis is now interpreted differently. Proceed with caution! More details about the update can be found at https://groups.google.com/g/lavaan/c/HSavF8oaW5M")
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

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan; showLineNumber: true }
	}

	RadioButtonGroup
	{
		title: qsTr("Data")
		name: "dataType"
		columns: 2
		RadioButton { value: "raw"; label: qsTr("Raw"); checked: true }
		RadioButton
		{
			value: "varianceCovariance"; label: qsTr("Variance-covariance matrix")
			IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 500 }
		}
	}

	DropDown
	{
		name: "samplingWeights"
		label: qsTr("Sampling weights")
		showVariableTypeIcon: true
		addEmptyValue: true
	}

	Section
	{
		title: qsTr("Model Options")
		Group
		{
			DropDown
			{
				name: "factorScaling"
				label: qsTr("Factor scaling")
				values:
				[
					{ label: qsTr("Factor loadings")	, value: "factorLoading"	},
					{ label: qsTr("Factor variance")	, value: "factorVariance"			},
					{ label: qsTr("Effects coding")		, value: "effectCoding"	},
					{ label: qsTr("None")				, value: "none"				}
				]
			}
			CheckBox 
			{ name: "meanStructure"
				label: qsTr("Mean structure")
				checked: eq_intercepts.checked || eq_means.checked || eq_thresholds.checked
				CheckBox { name: "latentInterceptFixedToZero";		label: qsTr("Latent means fixed to zero"); checked: !eq_means.checked }
				CheckBox { name: "manifestInterceptFixedToZero";	label: qsTr("Manifest intercepts fixed to zero") 											}
				CheckBox { name: "manifestMeanFixedToZero";				label: qsTr("Mean of manifest intercepts fixed to zero")							}
			}
			
			CheckBox { name: "orthogonal";						label: qsTr("Assume factors uncorrelated")						}
		}

		Group
		{

			CheckBox { name: "exogenousCovariateFixed";					label: qsTr("Exogenous covariate(s) fixed"); 			checked: true	}
			CheckBox { name: "residualSingleIndicatorOmitted";	label: qsTr("Residual single indicator omitted");	checked: true	}
			CheckBox { name: "residualVariance";								label: qsTr("Residual variances included");				checked: true	}
			CheckBox { name: "exogenousLatentCorrelation";			label: qsTr("Exogenous latents correlated");			checked: true	}
			CheckBox { name: "dependentCorrelation";						label: qsTr("Dependent variables correlated");		checked: true	}
			CheckBox { name: "threshold";												label: qsTr("Thresholds");												checked: true	}
			CheckBox { name: "scalingParameter";								label: qsTr("Scaling parameters");								checked: true	}
			CheckBox { name: "efaConstrained";									label: qsTr("EFA blocks constrained");						checked: true	}
		}
	}

	Section
	{
		title: qsTr("Estimation Options")

		Group
		{
			RowLayout 
			{
				DropDown
				{
					name: "estimator"
					label: qsTr("Estimator")
					id: estimator
					values: [
						{ value: "default", label: qsTr("Default")},
						{ value: "ml",			label: qsTr("ML")			},
						{ value: "gls",			label: qsTr("GLS")		},
						{ value: "wls",			label: qsTr("WLS")		},
						{ value: "uls",			label: qsTr("ULS")		},
						{ value: "dwls",		label: qsTr("DWLS")		},
						{ value: "dls",		  label: qsTr("DLS")		},
						{ value: "pml",			label: qsTr("PML")		},
						{ value: "mlm",			label: qsTr("MLM")		},
						{ value: "mlmv",		label: qsTr("MLMV")		},
						{ value: "mlmvs",		label: qsTr("MLMVS")	},
						{ value: "mlf",			label: qsTr("MLF")		},
						{ value: "mlr",			label: qsTr("MLR")		},
						{ value: "wlsm",		label: qsTr("WLSM")		},
						{ value: "wlsmv",		label: qsTr("WLSMV")	},
						{ value: "ulsm",		label: qsTr("ULSM")		},
						{ value: "ulsmv",		label: qsTr("ULSMV")	}
					]
				}
				HelpButton
				{
					toolTip: 					qsTr("Click for more information")
					helpPage:					"forQml/tooltipEstimators"
				}
			}
			
			DropDown
			{
				name: "modelTest"
				id: modTest
				label: qsTr("Model test")
				values: [
					{ value: "default", 								label: qsTr("Default")										},
					{ value: "standard",								label: qsTr("Standard")										},
					{ value: "satorraBentler",					label: qsTr("Satorra-Bentler")						},
					{ value: "yuanBentler",							label: qsTr("Yuan-Bentler")								},
					{ value: "yuanBentlerMplus",				label: qsTr("Yuan-Bentler Mplus")					},
					{ value: "meanAndVarianceAdjusted",	label: qsTr("Mean and variance adjusted")	},
					{ value: "scaledAndShifted",				label: qsTr("Scaled and shifted")					},
					{ value: "bollenStine",							label: qsTr("Bootstrap (Bollen-Stine)")		}, 
					{ value: "browneResidualAdf", 			label: qsTr("Browne residual based (ADF)")}, 
					{ value: "browneResidualNt", 				label: qsTr("Browne residual based (NT)")	}
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
			}

			DropDown
			{
				label: qsTr("Information matrix")
				name: "informationMatrix"
				values: [
					{ value: "default", 	 label: qsTr("Default")			},
					{ value: "expected", 	 label: qsTr("Expected") 		},
					{ value: "observed", 	 label: qsTr("Observed") 		},
					{ value: "firstOrder", label: qsTr("First order") }
				]
			}

			DropDown
			{
				label: qsTr("Standard errors")
				name: "errorCalculationMethod"
				id: errorCalc
				values: [
					{ value: "default", 	 				label: qsTr("Default")						},
					{ value: "standard",  	 			label: qsTr("Standard") 					},
					{ value: "robust", 	 					label: qsTr("Robust") 						},
					{ value: "robustHuberWhite", 	label: qsTr("Robust Huber-White") },
					{ value: "bootstrap", 				label: qsTr("Bootstrap")					}
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
			}
			DropDown {
				visible: errorCalc.value == "bootstrap"
				label: qsTr("     Type")
				name: "bootstrapCiType"
				values: [
						{ label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"	},
						{ label: qsTr("Percentile"),                value: "percentile"         		},
						{ label: qsTr("Normal theory"),             value: "normalTheory"         		}
				]
			}

			CIField {
				text: qsTr("Confidence intervals")
				name: "ciLevel"
			}


		}

		Group
		{
			id: missingG
			CheckBox{name: "standardizedVariable"; label: qsTr("Standardize variables before estimation"); checked: false}

			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
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

		Group
		{
			CheckBox { name: "additionalFitMeasures";	label: qsTr("Additional fit measures") }
			CheckBox { name: "rSquared";				label: qsTr("R-squared")							}
			CheckBox { name: "ave";						label: qsTr("Average variance extracted (AVE)")		}
			CheckBox { name: "htmt";					label: qsTr("Heterotrait-monotrait ratio (HTMT)")	}
			CheckBox { name: "reliability";				label: qsTr("Reliability measures")					}
			CheckBox { name: "mardiasCoefficient";		label: qsTr("Mardia's coefficient")					}
			CheckBox { name: "observedCovariance";		label: qsTr("Observed covariances")					}
			CheckBox { name: "impliedCovariance";		label: qsTr("Implied covariances")					}
			CheckBox { name: "residualCovariance";		label: qsTr("Residual covariances")					}
			CheckBox { name: "standardizedResidual"; 	label: qsTr("Standardized residuals")				}
		}

		Group
		{
			CheckBox
			{
				name: "standardizedEstimate"; label: qsTr("Standardized estimates");
				RadioButtonGroup
				{
						name: "standardizedEstimateType"
						RadioButton { value: "all"; 	label: qsTr("All"); checked: true	}
						RadioButton { value: "latents"; label: qsTr("Latents")	}
						RadioButton { value: "nox"; 	label: qsTr("Except exogenous covariates")		}
				}
			}
			CheckBox
			{
				name: "pathPlot";
				text: qsTr("Path diagram");
				checked: false
				CheckBox {
					name: "pathPlotParameter"
					text: qsTr("Show parameter estimates")
					checked: false
					CheckBox {
						name: "pathPlotParameterStandardized"
						text: qsTr("Standardized")
						checked: false
					}
				}
				CheckBox {
					name: "pathPlotLegend"
					text: qsTr("Show legend")
					checked: false
				}
			}
			CheckBox
			{
				name: "modificationIndex"
				label: qsTr("Modification indices")
				CheckBox
				{
					name: "modificationIndexHiddenLow"
					label: qsTr("Hide low indices")
					DoubleField
					{
						name: "modificationIndexThreshold"
						label: qsTr("Threshold")
						negativeValues: false
						decimals: 2
						defaultValue: 10
					}
				}
			}
			CheckBox 
			{ 
				name: "warnings";	
				label: qsTr("Show warnings")
			}

		}
	}

	Section
	{
		title: qsTr("Multigroup SEM")

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
			Group
			{
				visible: grpvar.value != ""
				id: constraints
				title: qsTr("Equality Constraints")
				CheckBox { id: eq_loadings; 			name: "equalLoading";				label: qsTr("Loadings")				}
				CheckBox { id: eq_intercepts; 			name: "equalIntercept";				label: qsTr("Intercepts")			}
				CheckBox { id: eq_residuals; 			name: "equalResidual";				label: qsTr("Residuals")			}
				CheckBox { id: eq_residualcovariances; 	name: "equalResidualCovariance";	label: qsTr("Residual covariances")	}
				CheckBox { id: eq_means; 				name: "equalMean";					label: qsTr("Means")				}
				CheckBox { id: eq_thresholds; 			name: "equalThreshold";				label: qsTr("Thresholds")			}
				CheckBox { id: eq_regressions; 			name: "equalRegression";			label: qsTr("Regressions")			}
				CheckBox { id: eq_variances; 			name: "equalLatentVariance";		label: qsTr("Latent variances")		}
				CheckBox { id: eq_lvcovariances; 		name: "equalLatentCovariance";		label: qsTr("Latent covariances")	}
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
		}
		
	}


	Section 
	{
		title: qsTr("Sensitivity Analysis")
		CheckBox
		{
			name: "sensitivityAnalysis"
			label: qsTr("Run sensitivity analysis")
			RadioButtonGroup
			{
				title: qsTr("Search algorithm")
				name: "searchAlgorithm"
				id: search
				RadioButton
				{
					value: "antColonyOptimization"
					label: qsTr("Ant colony optimization")
					checked: true 
					IntegerField { name: "numberOfAnts"; 			label: qsTr("Number of ants"); 					defaultValue: 10	}
					IntegerField { name: "sizeOfSolutionArchive"; 	label: qsTr("Size of the solution archive"); 	defaultValue: 100	}
					DoubleField { name: "convergenceRateThreshold";	label: qsTr("Convergence rate threshold");		defaultValue: 0.1;	negativeValues: false	}
				}
				RadioButton { value: "tabuSearch"; 				label: qsTr("Tabu search") 								}
			}
			DropDown
			{
				name: "optimizerFunction"
				label: qsTr("Optimizer function")
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
			}
			IntegerField { name: "maxIterations"; 	label: qsTr("Maximum number of iterations"); 	defaultValue: 1000	}
			SetSeed{}
		}
	}
}
