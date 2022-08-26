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
import JASP				1.0

Form
{

	columns: 1

	// The following part is used for spawning upgrade notifications about multigroup analysis
	Rectangle
	{
		visible:		myAnalysis !== null && myAnalysis.needsRefresh && grpvar.currentIndex !== 0 // if groupvar index is 0, there is no grouping variable -> no multigroup analysis
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

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan }
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
			IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 0 }
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
		title: qsTr("Output options")

		Group
		{
			CheckBox { name: "additionalFitMeasures";	label: qsTr("Additional fit measures")	}
			CheckBox { name: "rSquared";				label: qsTr("R-squared")				}
			CheckBox { name: "observedCovariance";		label: qsTr("Observed covariances")		}
			CheckBox { name: "impliedCovariance";		label: qsTr("Implied covariances")		}
			CheckBox { name: "residualCovariance";		label: qsTr("Residual covariances")		}
			CheckBox { name: "standardizedResidual"; 	label: qsTr("Standardized residuals")	}
			CheckBox { name: "mardiasCoefficient";		label: qsTr("Mardia's coefficient")		}
		}
		Group
		{
			CheckBox{name: "standardizedEstimate"; label: qsTr("Standardized estimates"); checked: false}
			CheckBox
			{
				name: "pathPlot";
				text: qsTr("Path diagram");
				checked: false
				CheckBox {
					name: "pathPlotParameter"
					text: qsTr("Show parameter estimates")
					checked: false
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
		}

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
			CheckBox { name: "meanStructure";					label: qsTr("Include mean structure")							}
			CheckBox { name: "manifestInterceptFixedToZero";	label: qsTr("Fix manifest intercepts to zero")					}
			CheckBox { name: "latentInterceptFixedToZero";		label: qsTr("Fix latent intercepts to zero");	checked: true	}
			CheckBox { name: "orthogonal";						label: qsTr("Assume factors uncorrelated")						}
		}

		Group
		{

			CheckBox { name: "exogenousCovariateFixed";			label: qsTr("Fix exogenous covariates"); 		checked: true	}
			CheckBox { name: "residualSingleIndicatorOmitted";	label: qsTr("Omit residual single indicator");	checked: true	}
			CheckBox { name: "residualVariance";				label: qsTr("Include residual variances");		checked: true	}
			CheckBox { name: "exogenousLatentCorrelation";		label: qsTr("Correlate exogenous latents");		checked: true	}
			CheckBox { name: "dependentCorrelation";			label: qsTr("Correlate dependent variables");	checked: true	}
			CheckBox { name: "threshold";						label: qsTr("Add thresholds");					checked: true	}
			CheckBox { name: "scalingParameter";				label: qsTr("Add scaling parameters");			checked: true	}
			CheckBox { name: "efaConstrained";					label: qsTr("Constrain EFA blocks");			checked: true	}
		}
	}

	Section
	{
		title: qsTr("Estimation options")

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
				RadioButton { value: "standard";	label: qsTr("Standard"); checked: true		}
				RadioButton { value: "robust";		label: qsTr("Robust")						}
				RadioButton
				{
					value: "bootstrap";	label: qsTr("Bootstrap")
					IntegerField
					{
						name: "bootstrapSamples"
						label: qsTr("Bootstrap samples")
						fieldWidth: 60
						defaultValue: 1000
						min: 1
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
			CheckBox{name: "standardizedVariable"; label: qsTr("Standardize variables before estimation"); checked: false}
			DropDown
			{
				name: "estimator"
				label: qsTr("Estimator")
				values: [
					{ value: "default",	label: qsTr("Auto") },
					{ value: "ml",		label: qsTr("ML")	},
					{ value: "gls",		label: qsTr("GLS")	},
					{ value: "wls",		label: qsTr("WLS")	},
					{ value: "uls",		label: qsTr("ULS")	},
					{ value: "dwls",	label: qsTr("DWLS")	},
					{ value: "pml",		label: qsTr("PML")	}
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
			}

			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				values:
				[
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

	Section
	{
		title: qsTr("Multigroup SEM")
		id: multigroup
		Group
		{
			DropDown
			{
				id: grpvar
				name: "group"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
			} // No model or source: it takes all variables per default
			Group
			{
				id: constraints
				title: qsTr("Equality Constraints")
				CheckBox { id: eq_loadings; 			name: "equalLoading";				label: qsTr("Loadings")				}
				CheckBox { id: eq_intercepts; 			name: "equalIntercept";				label: qsTr("Intercepts")			}
				CheckBox { id: eq_residuals; 			name: "equalResidual";				label: qsTr("Residuals")			}
				CheckBox { id: eq_residualcovariances; 	name: "equalResidualCovariance";	label: qsTr("Residual covariances")	}
				CheckBox { id: eq_means; 				name: "equalMean";					label: qsTr("Means")				}
				CheckBox { id: eq_thresholds; 			name: "equalThreshold";				label: qsTr("Threshold")			}
				CheckBox { id: eq_regressions; 			name: "equalRegression";			label: qsTr("Regressions")			}
				CheckBox { id: eq_variances; 			name: "equalLatentVariance";		label: qsTr("Latent variances")		}
				CheckBox { id: eq_lvcovariances; 		name: "equalLatentCovariance";		label: qsTr("Latent covariances")	}
			}

		}
		TextArea
		{
			name: "freeParameters"
			title: qsTr("Release constraints (one per line)")
			width: multigroup.width / 2
			height: constraints.height + grpvar.height
			textType: JASP.TextTypeLavaan
			visible: eq_loadings.checked || eq_intercepts.checked || eq_residuals.checked || eq_residualcovariances.checked || eq_means.checked || eq_thresholds.checked || eq_regressions.checked || eq_variances.checked || eq_lvcovariances.checked
		}
	}
}
