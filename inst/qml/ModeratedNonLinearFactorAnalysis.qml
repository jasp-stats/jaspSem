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
		allowedColumns:		["scale"]
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
				id: moderators
				allowedColumns:		["scale", "nominal"]
				title: qsTr("Effects")
				name: "moderators"
				rowComponentTitle: qsTr("Square   Cubic")
				rowComponent: RowLayout
				{
					CheckBox
					{
						name: "squaredEffect"
						id: squared
						onCheckedChanged:
						{
							if (checked)
							{
								var newValues1 = [rowValue  + qsTr("_squared")]
								for (var i = 0; i < plots.names.length; i++)
										newValues1.push(plots.names[i])
								plots.names = newValues1
							}
							else
							{
								var newValues1 = plots.names.filter(value => value !== (rowValue + qsTr("_squared")))
								plots.names = newValues1
							}
						}
					}
					CheckBox
					{
						name: "cubicEffect"
						id: cubic
						onCheckedChanged:
						{
							if (checked)
							{
								var newValues2 = [rowValue  + qsTr("_cubic")]
								for (var i = 0; i < plots.names.length; i++)
										newValues2.push(plots.names[i])
								plots.names = newValues2
							}
							else
							{
								var newValues2 = plots.names.filter(value => value !== (rowValue + qsTr("_cubic")))
								plots.names = newValues2
							}
						}
					}
				}

			}

			CheckBox {
				id: interactions
				name: "addInteractionTerms"
				enabled: moderators.columnsNames.length > 1
				label: qsTr("Add two-way interaction terms to model")
			}
		}

		Group 
		{
			title: qsTr("Assumption Check")
			CheckBox 
			{ 
				name: "fitPerGroup"; 
				checked: true ; 
				label: qsTr("Check model fit per group"); 
				id: fitPerGroup; 
				enabled: moderators.columnsNames != "" 
			}
			IntegerField 
			{ 
				name: "continuousVariableSplit"; 
				label: qsTr("Split continous variables into groups:");  
				defaultValue: 2; 
				enabled: fitPerGroup.checked; 
				min: 2 
			}
			CheckBox { name: "addGroupVar"; label: qsTr("Add group variable to data"); enabled: fitPerGroup.checked }
		}

		Group
		{
			title: qsTr("Global Invariance Testing")
			CheckBox { name: "configuralInvariance" ; checked: true ; label: qsTr("Configural"); id: configuralInvariance }
			CheckBox { name: "metricInvariance" ; checked: false ; label: qsTr("Metric"); id: metricInvariance }
			CheckBox { name: "scalarInvariance" ; checked: false ; label: qsTr("Scalar"); id: scalarInvariance }
			CheckBox { name: "strictInvariance" ; checked: false ; label: qsTr("Strict"); id: strictInvariance }
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

		property var interactionPairs: interactions.checked ? combinePairs(moderators.columnsNames) : null
		property var combinedSources: moderators.columnsNames.concat(interactionPairs);		

   // now also add the square and cubic effects 
		ComponentsList
		{
			title: qsTr("Invariance Tests")
			name: "something"
			values: modOpts.combinedSources.filter(value => value !== null)
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

		// RadioButtonGroup 
		// {
		// 	title: qsTr("Factor Scaling")
		// 	RadioButton { value: "factorVariance"; label: qsTr("Factor variance") }
		// 	RadioButton { value: "factorLoading"; label: qsTr("Factor loading") }
		// }
		CheckBox { name: "factorsUncorrelated"; label: qsTr("Assume factors uncorrelated")   }

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
		id: plots

		// property var names: moderators.columnsNames
		property var names: []
		
		TabView 
		{
			values: [
				configuralInvariance.checked ? qsTr("Configural") : null,
				metricInvariance.checked ? qsTr("Metric") : null,
				scalarInvariance.checked ? qsTr("Scalar") : null,
				strictInvariance.checked ? qsTr("Strict") : null
			].filter(value => value !== null) // Dynamically set values based on checkboxes
			name: "plotModelList"
			addItemManually: false
			rowComponent: TabView 
			{
				name: "plotParameterList"
				addItemManually: false
				values: [qsTr("Loadings"), qsTr("Intercepts"), qsTr("Residual variances"), qsTr("Factor variances"), qsTr("Factor means"), qsTr("Factor covariances")]
				rowComponent: ComponentsList
				{
					name: "plotItemList"
					addItemManually: false
					headerLabels: [qsTr("Moderator 1"), qsTr("Moderator 2"), qsTr("Display plot")]
					source: factors.name
					rowComponent: RowLayout
					{
						Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
						DropDown
						{
							name: "plotMod1"
							id: plotMod1
							// source: [moderators]
							source: [moderators, {values: plots.names}, {values: modOpts.interactionPairs}]

							addEmptyValue: true
						}
						DropDown
						{
							name: "plotMod2"
							id: plotMod2
							addEmptyValue: true
							source: plotMod1
							// source: [{id: plotMod1, discard: {values: [plotMod1.currentValue]}}]
						}

						CheckBox { name: "includePlot"; enabled: plotMod1.currentValue !== "" || plotMod2.currentValue !== "" }
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")

	}
}


