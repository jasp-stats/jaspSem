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
import JASP
import JASP.Controls

Form
{

	FactorsForm
	{
		id:					factors
		name:				"factors"
		initNumberFactors:	1
		allowedColumns:		["ordinal", "scale"]
		allowTypeChange:	true
		keepAvailableVariables: true
	}

	Section
	{
		title: qsTr("Moderation")
		expanded: true
		id: mod
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.75
			AvailableVariablesList 		 
			{	
				title: qsTr("Moderator Variables")
				name: "moderatorVars"
				source: [{ isDataSetVariables: true, discard: factors.name }]
			}
			AssignedVariablesList
			{	
				title: qsTr("Effects:")
				name: "moderators"
				id: moderators
				rowComponentTitle: qsTr("Square   Cubic")
				rowComponent: RowLayout
					{
						CheckBox { name: "squaredEffect" }
						CheckBox { name: "cubicEffect" }
					}
			}
			CheckBox { name: "addInteractionTerms" ; checked: true; label: qsTr("Add two-way interaction terms to model") }
		}

		Group 
		{
			title: qsTr("Assumption Check")
			CheckBox { name: "fitPerGroup" ; checked: true ; label: qsTr("Check model fit per group"); id: fitPerGroup; enabled: moderators.columnsNames != "" }
			IntegerField { name: "continuousVariableSplit"; label: qsTr("Split continous variables into groups:");  defaultValue: 2; enabled: fitPerGroup.checked; min: 2 }
			CheckBox { name: "addGroupVar"; label: qsTr("Add group variable to data"); enabled: fitPerGroup.checked }
		}

		Group
		{
			title: qsTr("Global Invariance Testing")
			CheckBox { name: "configuralInvariance" ; checked: true ; label: qsTr("Configural") }
			CheckBox { name: "metricInvariance" ; checked: false ; label: qsTr("Metric") }
			CheckBox { name: "scalarInvariance" ; checked: false ; label: qsTr("Scalar") }
			CheckBox { name: "strictInvariance" ; checked: false ; label: qsTr("Strict") }
		}
		
	}

	Section
	{
		id: modOpts
		title: qsTr("Moderation Options")

		function combinePairs(values) {
			var pairs = [];
			for (var i = 0; i < values.length; i++) {
					for (var j = i + 1; j < values.length; j++) {
									pairs.push(values[i] + ":" + values[j]);
					}
			}
			return pairs;
		}
		property var interactionPairs: combinePairs(moderators.columnsNames);
		property var combinedSources: moderators.columnsNames.concat(interactionPairs);

   // now also add the square and cubic effects 
		ComponentsList
		{
			title: qsTr("Invariance Tests")
			name: "something"
			values: modOpts.combinedSources
			addItemManually: false
			headerLabels: [qsTr("Factor 1")]
			rowComponent: RowLayout
			{
				Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
				DropDown 
				{ 
					name: "invarianceTest"
					values: [
						{ label: qsTr("Factor mean"), value: "latentMean" },
						{ label: qsTr("Factor variance"), value: "latentVariance" },
						{ label: qsTr("Factor loadings"), value: "loadings" }, 
						{ label: qsTr("Item intercepts"), value: "intercepts"},
						{ label: qsTr("Residual variances"), value: "residualVariances"}
					]
					addEmptyValue: true
				}
			}
		}
	}

	Section
	{
		title: qsTr("Model Options")

		CheckBox { label: qsTr("Assume factors uncorrelated") ; name: "factorsUncorrelated"    }

	}

	Section
	{
		title: qsTr("Output Options")
		
		Group
		{
			title: qsTr("Parameter Estimates")
			DoubleField
			{
				name: "alphaLevel"
				label: qsTr("Significance level")
				negativeValues: false
				decimals: 4
				defaultValue: 0.05
			}
			CheckBox { name: "loadingEstimates"; label: qsTr("Loadings") }
			CheckBox { name: "interceptEstimates"; label: qsTr("Intercepts") }
			CheckBox { name: "residualVarianceEstimates"; label: qsTr("Residual variances") }
			CheckBox { name: "factorVarianceEstimates"; label: qsTr("Factor variances") }
			CheckBox { name: "factorMeanEstimates"; label: qsTr("Factor means") }
			CheckBox { name: "factorCovarianceEstimates"; label: qsTr("Factor covariances") }

		}

		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovarianceMatrix"		}
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovarianceMatrix"		}
			CheckBox {
				label: qsTr("Modification indices")
				name: "modificationIndices"
				DoubleField {
					label: qsTr("Cutoff")
					name: "modificationIndicesCutoff"
					min: 0
					defaultValue: 3.84
				}
			}
			CheckBox { label: qsTr("Show syntax")         ; name: "showSyntax" }
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group 
		{
			title: qsTr("Plot Individual Parameters")
			
		}

	}

	Section
	{
		title: qsTr("Advanced")

		Group {
			DropDown
			{
				name: "packageMimiced"
				label: qsTr("Mimic")
				values: [
					{ label: qsTr("Lavaan"), value: "lavaan" },
					{ label: qsTr("Mplus"), value: "mplus" },
					{ label: qsTr("EQS"), value: "eqs" }
				]
			}
			RowLayout 
			{
				DropDown
				{
					name: "estimator"
					label: qsTr("Estimator")
					id: estimator
					values: [
						{ label: qsTr("Default"), value: "default" },
						{ label: qsTr("ML"), value: "ml" },
						{ label: qsTr("GLS"), value: "gls" },
						{ label: qsTr("WLS"), value: "wls" },
						{ label: qsTr("ULS"), value: "uls" },
						{ label: qsTr("DWLS"), value: "dwls" },
						{ label: qsTr("DLS"), value: "dls" },
						{ label: qsTr("PML"), value: "pml" },
						{ label: qsTr("MLM"), value: "mlm" },
						{ label: qsTr("MLMV"), value: "mlmv" },
						{ label: qsTr("MLMVS"), value: "mlmvs" },
						{ label: qsTr("MLF"), value: "mlf" },
						{ label: qsTr("MLR"), value: "mlr" },
						{ label: qsTr("WLSM"), value: "wlsm" },
						{ label: qsTr("WLSMV"), value: "wlsmv" },
						{ label: qsTr("ULSM"), value: "ulsm" },
						{ label: qsTr("ULSMV"), value: "ulsmv" }
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
				label: qsTr("Standard errors")
				name: "seType"
				id: errorCalc
				values: [
					{ label: qsTr("Default"), 				value: "default"						},
					{ label: qsTr("Standard"),  			value: "standard" 					},
					{ label: qsTr("Robust"), 				value: "robust" 						},
					{ label: qsTr("Bootstrap"), 			value: "bootstrap"					}
				]
			}
			IntegerField
			{
				visible: errorCalc.value == "bootstrap"
				name: "bootstrapSamples"
				label: qsTr("     Bootstrap samples")
				defaultValue: 1000
				min: 100
				max: 1000000
			}
			CIField { text: qsTr("     CI level"); name: "ciLevel" }
			
			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				values: factors.columnsTypes.includes("ordinal") ? [
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("Pairwise")			, value: "pairwise"			},
				] : [
					{ label: qsTr("FIML")				, value: "fiml"				},
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("Pairwise")			, value: "pairwise"			},
					{ label: qsTr("Two-stage")			, value: "twoStage"			},
					{ label: qsTr("Robust two-stage")	, value: "twoStageRobust"	}
				]
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Standardization")
			name: "standardized"
			RadioButton { label: qsTr("None");										value: "none"; checked: true	}
			RadioButton { label: qsTr("Latents");									value: "latentVariables"		}
			RadioButton { label: qsTr("All");											value: "all"					}
			RadioButton { label: qsTr("No Exogenous Covariates");	value: "noExogenousCovariates"	}
		}

		Group
		{
			title: qsTr("Options")
			debug: true
			CheckBox { label: qsTr("Fix manifest intercepts to zero")	; 	name: "manifestInterceptsFixedToZero" }
			CheckBox { label: qsTr("Fix latent intercepts to zero")   	; name: "latentInterceptsFixedToZero";		checked: true }
			CheckBox { label: qsTr("Omit residual single indicator")  	; name: "residualSingleIndicatorOmitted";	checked: true }
			CheckBox { label: qsTr("Residual variances")              	; name: "residualVariances";				checked: true }
			CheckBox { label: qsTr("Correlate exogenous latents")     	; name: "exogenousLatentsCorrelated";		checked: true }
			CheckBox { label: qsTr("Add thresholds")                 	; 	name: "thresholds";						checked: true }
			CheckBox { label: qsTr("Add scalings parameters")         	; name: "scalingParamaters";				checked: true }
			CheckBox { label: qsTr("Correlate dependent variables")   	; name: "dependentVariablesCorrelated";		checked: true }
		}
	}
}


