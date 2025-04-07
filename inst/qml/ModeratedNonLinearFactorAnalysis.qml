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

	// function for getting the values for the plots
	function getValues(inValue, rwValue, factorCount) {
		if (inValue == qsTr("Configural")) {
			if (rwValue == qsTr("Indicators")) {
				return [qsTr("Loadings"), qsTr("Intercepts"), qsTr("Residual variances")];
			} 
			if (factorCount == 2) {
				return [qsTr("Covariances")];
			} 
			return [];
		} 
		if (inValue == qsTr("Metric")) {
			if (rwValue == qsTr("Indicators")) {
				return [qsTr("Intercepts"), qsTr("Residual variances")];
			} 
			if (factorCount == 2) {
				return [qsTr("Variances"), qsTr("Covariances")];
			}
			return [qsTr("Variances")];
		} 
		if (inValue == qsTr("Scalar")) {
			if (rwValue == qsTr("Indicators")) {
				return [qsTr("Residual variances")];
			} 
			if (factorCount == 2) {
				return [qsTr("Variances"), qsTr("Means"), qsTr("Covariances")];
			} 
			return [qsTr("Variances"), qsTr("Means")];
		} 
		if (inValue == qsTr("Strict")) {
			if (rwValue == qsTr("Indicators")) {
				return [];
			} 
			if (factorCount == 2) {
				return [qsTr("Covariances")];
			} 
			return [];
		} 
		return [];
	}

	function concatFactorTitles(factorTitles) {
		var result = [];
		for (var i = 0; i < factorTitles.length; i++) {
			result.push(factorTitles[i].label);
		}
		result = result.join(" <-> ");
		return [result];
	}

	function combinePairs(values) {
		var pairs = [];
		for (var i = 0; i < values.length; i++) {
				for (var j = i + 1; j < values.length; j++) {
								pairs.push(values[i] + ":" + values[j]);
				}
		}
		return pairs;
	}

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
		expanded: false
		id: mod
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.65
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
		}

		Group 
		{
			title: qsTr("Assumption Check")
			CheckBox 
			{ 
				name: "fitPerGroup"; 
				checked: false ; 
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
			preferredWidth: form.width / 2.5

			ComponentsList 
			{
				visible: moderators.columnsNames.length > 1
				title: qsTr("Interaction Terms")
				name: "moderatorInteractions"
				id: interactions
				addItemManually: false
				values: combinePairs(moderators.columnsNames)
				headerLabels: [qsTr("Include")]
				rowComponent: RowLayout
				{
					Text{ text: rowValue; Layout.preferredWidth: 150*jaspTheme.uiScale }
					CheckBox { name: "includeInteraction"; id: includeInteraction }
				}
			}
		
		}
		
	}

	Section
	{
		id: invOpts
		title: qsTr("Invariance Options")

		Group
		{
			columns: 4
			title: qsTr("Invariance Tests")
			CheckBox { name: "configuralInvariance" ; checked: true ; label: qsTr("Configural"); id: configuralInvariance }
			CheckBox { name: "metricInvariance" ; checked: false ; label: qsTr("Metric"); id: metricInvariance }
			CheckBox { name: "scalarInvariance" ; checked: false ; label: qsTr("Scalar"); id: scalarInvariance }
			CheckBox { name: "strictInvariance" ; checked: false ; label: qsTr("Strict"); id: strictInvariance }
		}

	// 	property var interactionPairs: interactions.checked ? combinePairs(moderators.columnsNames) : null
	// 	property var combinedSources: moderators.columnsNames.concat(interactionPairs);		

  //  // now also add the square and cubic effects 
	// 	ComponentsList
	// 	{
	// 		title: qsTr("Invariance Tests")
	// 		name: "something"
	// 		values: modOpts.combinedSources.filter(value => value !== null)
	// 		addItemManually: false
	// 		headerLabels: [qsTr("Factor 1")]
	// 		rowComponent: RowLayout
	// 		{
	// 			Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
	// 			DropDown 
	// 			{ 
	// 				name: "invarianceTest"
	// 				values: [
	// 					{ label: qsTr("Factor mean"), value: "latentMean" },
	// 					{ label: qsTr("Factor variance"), value: "latentVariance" },
	// 					{ label: qsTr("Factor loadings"), value: "loadings" }, 
	// 					{ label: qsTr("Item intercepts"), value: "intercepts"},
	// 					{ label: qsTr("Residual variances"), value: "residualVariances"}
	// 				]
	// 				addEmptyValue: true
	// 			}
	// 		}
	// 	}
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
			id: firstTab
			addItemManually: false
			rowComponent: TabView 
			{
				id: secondTab
				property var invValue: rowValue 
				name: "plotTypeList"
				addItemManually: false
				values: [qsTr("Indicators"), qsTr("Factors")]
				rowComponent: TabView 
				{
					id: thirdTab
					property var typeValue: rowValue
					name: "plotParameterList"
					addItemManually: false

					values: getValues(secondTab.invValue, rowValue, factors.factorsTitles.length)
				
					rowComponent: ComponentsList
					{
						name: "plotItemList"
						addItemManually: false
						headerLabels: [qsTr("Moderator 1"), qsTr("Moderator 2"), qsTr("Display plot")]
						source: thirdTab.typeValue == qsTr("Indicators") ? factors.name : (rowValue == qsTr("Covariances") ? {values: concatFactorTitles(factors.factorsTitles)} : {values: factors.factorsTitles})
						
						rowComponent: RowLayout
						{
							Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
							DropDown
							{
								name: "plotMod1"
								id: plotMod1
								// source: [moderators]
								source: [moderators, {values: plots.names}, {name: "moderatorInteractions", condition: "includeInteraction"}]

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
	}

	Section
	{
		title: qsTr("Advanced")

	}
}


