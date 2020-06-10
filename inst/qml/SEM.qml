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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.12
import JASP.Controls 1.0

Form
{
	usesJaspResults: true
	columns: 1

	TabView
	{
		id: models
		name: "models"
		maximumItems: 9
		newItemName: qsTr("Model 1")
		optionKey: "modelName"

		content: TextArea { name: "syntax"; width: models.width; textType: "lavaan"}
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
			CheckBox { name: "outputAdditionalFitMeasures";				label: qsTr("Additional fit measures")				}
			CheckBox { name: "outputRSquared";                          label: qsTr("R-squared")                            }
			CheckBox { name: "outputFittedCovarianceCorrelations";		label: qsTr("Fitted covariances / correlations")	}
			CheckBox { name: "outputObservedCovarianceCorrelations";	label: qsTr("Observed covariances / correlations")	}
			CheckBox { name: "outputResidualCovarianceCorrelations";	label: qsTr("Residual covariances / correlations")	}
			CheckBox { name: "outputMardiasCoefficients";				label: qsTr("Mardia's coefficient")					}
			CheckBox
			{
				name: "outputModificationIndices";						label: qsTr("Modification indices")
				CheckBox
				{
					name: "outputModificationIndicesHideLowIndices";	label: qsTr("Hide low indices")
					IntegerField { name: "outputModificationIndicesHideLowIndicesThreshold"; label: qsTr("Threshold"); defaultValue: 10 }
				}
			}
		}

		CheckBox 
		{
			name: "addPathDiagram";
			text: qsTr("Path diagram");
			checked: false
			CheckBox {
				name: "outputpathdiagramstandardizedparameter"
				text: qsTr("Show standardized parameters")
				checked: false
			}
		}
	}

	Section
	{
		title: qsTr("Model Options")
		CheckBox { name: "meanstructure";	label: qsTr("Include mean structure")							}
		CheckBox { name: "int.ov.fixed";	label: qsTr("Fix manifest intercepts to zero")					}
		CheckBox { name: "int.lv.fixed";	label: qsTr("Fix latent intercepts to zero");	checked: true	}
		CheckBox { name: "orthogonal";		label: qsTr("Assume factors uncorrelated")						}
		CheckBox { name: "fixed.x";			label: qsTr("Fix exogenous covariates"); 		checked: true	}
		DropDown
		{
			name: "factorStandardisation"
			label: qsTr("Factor scaling")
			values:
			[
				{ label: qsTr("Factor loadings")	, value: "auto.fix.first"	},
				{ label: qsTr("Residual variance")	, value: "std.lv"			},
				{ label: qsTr("Effects coding")		, value: "effect.coding"	}, 
				{ label: qsTr("None")				, value: "none"				}
			]
		}
		CheckBox { name: "auto.fix.single";		label: qsTr("Omit residual single indicator");	checked: true	}
		CheckBox { name: "auto.var";			label: qsTr("Residual variances");				checked: true	}
		CheckBox { name: "auto.cov.lv.x";		label: qsTr("Correlate exogenous latents");		checked: true	}
		CheckBox { name: "auto.cov.y";			label: qsTr("Correlate dependent variables");	checked: true	}
		CheckBox { name: "auto.th";				label: qsTr("Add thresholds");					checked: true	}
		CheckBox { name: "auto.delta";			label: qsTr("Add scaling parameters");			checked: true	}
		CheckBox { name: "auto.efa";			label: qsTr("Constrain EFA blocks");			checked: true	}

	}

	Section
	{
		title: qsTr("Data options")
		
		CheckBox{name: "std.ov"; label: qsTr("Standardize variables before analysis"); checked: false}

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
	}

	Section
	{
		title: qsTr("Estimation options")

		RadioButtonGroup
		{
			title: qsTr("Error Calculation")
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
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Information")
			name: "information"
			RadioButton { value: "expected"; label: qsTr("Expected"); checked: true }
			RadioButton { value: "observed"; label: qsTr("Observed"); 				}
		}
		
		DropDown
		{
			name: "estimator"
			label: qsTr("Estimator")
			values: [
				{ value: "automatic",	label: qsTr("Auto") },
				{ value: "ML",			label: qsTr("ML")	},
				{ value: "GLS",			label: qsTr("GLS")	},
				{ value: "WLS",			label: qsTr("WLS")	},
				{ value: "ULS",			label: qsTr("ULS")	},
				{ value: "DWLS",		label: qsTr("DWLS")	},
				{ value: "PML",			label: qsTr("PML")	}
			]
		}

		DropDown
		{
			name: "emulation"
			title: qsTr("Emulation")
			values: [
				{ value: "lavaan",	label: qsTr("None") 	},
				{ value: "Mplus",	label: qsTr("Mplus") 	},
				{ value: "EQS",		label: qsTr("EQS") 		}
			] 
		}
	}
	
	Section
	{
		title: qsTr("Multigroup SEM")
		
		DropDown 
		{ 
			name: "groupingVariable"
			label: qsTr("Grouping Variable")
			showVariableTypeIcon: true
			addEmptyValue: true
		} // No model or source: it takes all variables per default

		Group
		{
			title: qsTr("Equality Constraints")
			CheckBox { name: "eq_loadings";				label: qsTr("Loadings")				}
			CheckBox { name: "eq_intercepts";			label: qsTr("Intercepts")			}
			CheckBox { name: "eq_residuals";			label: qsTr("Residuals")			}
			CheckBox { name: "eq_residualcovariances";	label: qsTr("Residual covariances")	}
			CheckBox { name: "eq_means";				label: qsTr("Means")				}
			CheckBox { name: "eq_thresholds";			label: qsTr("Threshold")			}
			CheckBox { name: "eq_regressions";			label: qsTr("Regressions")			}
			CheckBox { name: "eq_variances";			label: qsTr("Latent variances")		}
			CheckBox { name: "eq_lvcovariances";		label: qsTr("Latent covariances")	}
		}
	}
}
