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
		optionKey: "modelName"

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan }
	}

	RadioButtonGroup
	{
		title: qsTr("Data")
		name: "Data"
		columns: 2
		RadioButton { value: "raw"; label: qsTr("Raw"); checked: true }
		RadioButton
		{
			value: "varcov"; label: qsTr("Variance-covariance matrix")
			IntegerField { name: "SampleSize"; label: qsTr("Sample size"); defaultValue: 0 }
		}
	}

	DropDown
	{
		name: "sampling.weights"
		label: qsTr("Sampling weights")
		showVariableTypeIcon: true
		addEmptyValue: true
	}
  
	Section
	{
		title: qsTr("Output options")

		Group
		{	
			CheckBox { name: "outputAdditionalFitMeasures";	label: qsTr("Additional fit measures")	}
			CheckBox { name: "outputRSquared";				label: qsTr("R-squared")				}
			CheckBox { name: "outputObservedCovariances";	label: qsTr("Observed covariances")		}
			CheckBox { name: "outputImpliedCovariances";	label: qsTr("Implied covariances")		}
			CheckBox { name: "outputResidualCovariances";	label: qsTr("Residual covariances")		}
			CheckBox { name: "outputStandardizedResiduals"; label: qsTr("Standardized residuals")	}
			CheckBox { name: "outputMardiasCoefficients";	label: qsTr("Mardia's coefficient")		}
		}
		Group
		{
			CheckBox{name: "std"; label: qsTr("Standardized estimates"); checked: false}
			CheckBox 
			{
				name: "outputPathPlot";
				text: qsTr("Path diagram");
				checked: false
				CheckBox {
					name: "pathPlotPar"
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
				name: "outputModificationIndices"
				label: qsTr("Modification indices")
				CheckBox
				{
					name: "miHideLow"
					label: qsTr("Hide low indices")
					DoubleField 
					{ 
						name: "miThreshold"
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
				name: "factorStandardisation"
				label: qsTr("Factor scaling")
				values:
				[
					{ label: qsTr("Factor loadings")	, value: "auto.fix.first"	},
					{ label: qsTr("Factor variance")	, value: "std.lv"			},
					{ label: qsTr("Effects coding")		, value: "effect.coding"	}, 
					{ label: qsTr("None")				, value: "none"				}
				]
			}
			CheckBox { name: "meanstructure";	label: qsTr("Include mean structure")							}
			CheckBox { name: "int.ov.fixed";	label: qsTr("Fix manifest intercepts to zero")					}
			CheckBox { name: "int.lv.fixed";	label: qsTr("Fix latent intercepts to zero");	checked: true	}
			CheckBox { name: "orthogonal";		label: qsTr("Assume factors uncorrelated")						}
		}
		
		Group
		{
			
			CheckBox { name: "fixed.x";				label: qsTr("Fix exogenous covariates"); 		checked: true	}
			CheckBox { name: "auto.fix.single";		label: qsTr("Omit residual single indicator");	checked: true	}
			CheckBox { name: "auto.var";			label: qsTr("Include residual variances");		checked: true	}
			CheckBox { name: "auto.cov.lv.x";		label: qsTr("Correlate exogenous latents");		checked: true	}
			CheckBox { name: "auto.cov.y";			label: qsTr("Correlate dependent variables");	checked: true	}
			CheckBox { name: "auto.th";				label: qsTr("Add thresholds");					checked: true	}
			CheckBox { name: "auto.delta";			label: qsTr("Add scaling parameters");			checked: true	}
			CheckBox { name: "auto.efa";			label: qsTr("Constrain EFA blocks");			checked: true	}
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
				name: "information"
				values: [
					{ value: "expected", label: qsTr("Expected") },
					{ value: "observed", label: qsTr("Observed") }
				] 
			}

			RadioButtonGroup
			{
				title: qsTr("Error calculation")
				name: "se"
				RadioButton { value: "standard";	label: qsTr("Standard"); checked: true		}
				RadioButton { value: "robust";		label: qsTr("Robust")						}
				RadioButton
				{
					value: "bootstrap";	label: qsTr("Bootstrap")
					IntegerField
					{
						name: "errorCalculationBootstrapSamples"
						label: qsTr("Bootstrap samples")
						fieldWidth: 60
						defaultValue: 1000
						min: 1
					}
					DropDown {
                        label: qsTr("Type")
                        name: "bootCItype"
                        values: [
                            { label: qsTr("Bias-corrected percentile"), value: "bca.simple"   },
                            { label: qsTr("Percentile"),                value: "perc"         },
                            { label: qsTr("Normal theory"),             value: "norm"         }
                        ]
                    }
				}
			}
			
			CIField {
				text: qsTr("Confidence intervals")
				name: "ciWidth"
			}

			
		}
		
		Group 
		{
			CheckBox{name: "std.ov"; label: qsTr("Standardize variables before estimation"); checked: false}
			DropDown
			{
				name: "estimator"
				label: qsTr("Estimator")
				values: [
					{ value: "default",	label: qsTr("Auto") },
					{ value: "ML",		label: qsTr("ML")	},
					{ value: "GLS",		label: qsTr("GLS")	},
					{ value: "WLS",		label: qsTr("WLS")	},
					{ value: "ULS",		label: qsTr("ULS")	},
					{ value: "DWLS",	label: qsTr("DWLS")	},
					{ value: "PML",		label: qsTr("PML")	}
				]
			}

			DropDown
			{
				name: "test"
				label: qsTr("Model test")
				values: [
					{ value: "default",				label: qsTr("Auto") 						},
					{ value: "standard",			label: qsTr("Standard")						},
					{ value: "Satorra.Bentler",		label: qsTr("Satorra-Bentler")				},
					{ value: "Yuan.Bentler",		label: qsTr("Yuan-Bentler")					},
					{ value: "mean.var.adjusted",	label: qsTr("Mean and Variance adjusted")	},
					{ value: "scaled.shifted",		label: qsTr("Scaled and shifted")			},
					{ value: "Bollen.Stine",		label: qsTr("Bootstrap (Bollen-Stine)")		}
				]
			}

			DropDown
			{
				name: "missing"
				label: qsTr("Missing data handling")
				values:
				[
					{ label: qsTr("FIML")				, value: "ml"				},
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("Pairwise")			, value: "pairwise"			},
					{ label: qsTr("Two-stage")			, value: "two.stage"		}, 
					{ label: qsTr("Robust two-stage")	, value: "robust.two.stage"	},
					{ label: qsTr("Doubly robust")		, value: "doubly.robust"	},
				]
			}

			DropDown
			{
				name: "emulation"
				label: qsTr("Emulation")
				values: [
					{ value: "lavaan",	label: qsTr("None") 	},
					{ value: "Mplus",	label: qsTr("Mplus") 	},
					{ value: "EQS",		label: qsTr("EQS") 		}
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
				name: "groupingVariable"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
			} // No model or source: it takes all variables per default
			Group
			{
				id: constraints
				title: qsTr("Equality Constraints")
				CheckBox { id: eq_loadings; 			name: "eq_loadings";			label: qsTr("Loadings")				}
				CheckBox { id: eq_intercepts; 			name: "eq_intercepts";			label: qsTr("Intercepts")			}
				CheckBox { id: eq_residuals; 			name: "eq_residuals";			label: qsTr("Residuals")			}
				CheckBox { id: eq_residualcovariances; 	name: "eq_residualcovariances";	label: qsTr("Residual covariances")	}
				CheckBox { id: eq_means; 				name: "eq_means";				label: qsTr("Means")				}
				CheckBox { id: eq_thresholds; 			name: "eq_thresholds";			label: qsTr("Threshold")			}
				CheckBox { id: eq_regressions; 			name: "eq_regressions";			label: qsTr("Regressions")			}
				CheckBox { id: eq_variances; 			name: "eq_variances";			label: qsTr("Latent variances")		}
				CheckBox { id: eq_lvcovariances; 		name: "eq_lvcovariances";		label: qsTr("Latent covariances")	}
			}
			
		}
		TextArea
		{
			name: "group.partial"
			title: qsTr("Release constraints (one per line)")
			width: multigroup.width / 2
			height: constraints.height + grpvar.height
			textType: JASP.TextTypeSource
			separator: [";", "\n", ","]
			visible: eq_loadings.checked || eq_intercepts.checked || eq_residuals.checked || eq_residualcovariances.checked || eq_means.checked || eq_thresholds.checked || eq_regressions.checked || eq_variances.checked || eq_lvcovariances.checked
		}			
	}
}
