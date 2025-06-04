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
	function getValuesModOptions(inValue, rwValue, factorCount) {
		if (rwValue == "indicators") {
			return [{value: "loadings", label: qsTr("Loadings")}, {value: "intercepts", label: qsTr("Intercepts")}, {value: "residualVariances", label: qsTr("Residual variances")}];
		} 
		if (inValue == "configuralInvariance" && factorCount == 2) {
			return [{value: "covariances", label: qsTr("Covariances")}];
		}
		if (inValue == "metricInvariance") {
			if (factorCount == 2) {
				return [{value: "variances", label: qsTr("Variances")}, {value: "covariances", label: qsTr("Covariances")}];
			}
			return [{value: "variances", label: qsTr("Variances")}];
		} 
		if (inValue == "scalarInvariance" || inValue == "strictInvariance") {
			if (factorCount == 2) {
				return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}, {value: "covariances", label: qsTr("Covariances")}];
			} 
			return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}];
		} 
		return [];
	}

	function concatFactorTitles(factorTitles) {
		var resultLabel = [];
		for (var i = 0; i < factorTitles.length; i++) {
			resultLabel.push(factorTitles[i].label);
		}
		resultLabel = resultLabel.join(":");
		return [resultLabel];
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
		expanded: true
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
			CheckBox
			{
				id: 						syncAnalysisBox
				name: 					"syncAnalysisBox"
				label: 					qsTr("<b>Start/Sync Analysis</b>")
				checked: 				false
				Component.onCompleted:
				{
						background.color = "#ffcb98"
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

		ColumnLayout
		{
			Layout.alignment: Qt.AlignRight

			ComponentsList 
			{
				visible: moderators.columnsNames.length > 1
				preferredWidth: form.width * 0.4
				title: qsTr("Interaction Terms")
				name: "moderatorInteractions"
				id: interactions
				addItemManually: false
				values: combinePairs(moderators.columnsNames)
				headerLabels: [qsTr("Include")]
				rowComponent: RowLayout
				{
					Text { text: rowValue; Layout.preferredWidth: 150 * jaspTheme.uiScale }
					CheckBox { name: "includeInteraction"; id: includeInteraction }
				}
			}
		}
	}

	Section
	{
		id: invOpts
		title: qsTr("Moderation Options")

		// property var names: []

		Group
		{
			columns: 1
			title: qsTr("Invariance Tests")
			CheckBox { name: "configuralInvariance" ; checked: true ; label: qsTr("Configural"); id: configuralInvariance }
			CheckBox { name: "metricInvariance" ; checked: false ; label: qsTr("Metric"); id: metricInvariance }
			CheckBox { name: "scalarInvariance" ; checked: false ; label: qsTr("Scalar"); id: scalarInvariance }
			CheckBox { name: "strictInvariance" ; checked: false ; label: qsTr("Strict"); id: strictInvariance }
		}

		property var firstLayerValues: [configuralInvariance, metricInvariance, scalarInvariance, strictInvariance].filter(x => x.checked).map(x => ({value: x.name, label: x.label}))
		
		TabView 
		{
			Layout.columnSpan: 1
			preferredWidth: 100
			Layout.fillWidth: true
			values: invOpts.firstLayerValues
			title: qsTr("Include Individual Moderations")
			name: "moderationIncludeList"
			optionKey: "keyValue"
			optionKeyLabel: "keyLabel"
			id: modFirstLayer
			addItemManually: false
			rowComponent: TabView 
			{
				id: modSecondLayer
				property string modInvValue: rowValue
				property string modInvLable: rowLabel
				name: "modTypeList"
				addItemManually: false
				values: [{value: "indicators", label: qsTr("Indicators")}, {value: "factors", label: qsTr("Factors")}]
				optionKey: "keyValue"
				optionKeyLabel: "keyLabel"
				rowComponent: TabView 
				{
					id: modThirdLayer
					property string modTypeValue: rowValue
					property string modTypeLabel: rowLabel
					name: "modParameterList"
					addItemManually: false
					optionKey: "keyValue"
					optionKeyLabel: "keyLabel"
					values: getValuesModOptions(modSecondLayer.modInvValue, rowValue, factors.factorsTitles.length)
					rowComponent: ComponentsList
					{
						id: modFourthLayer
						property string modParamValue: rowValue
						property string modParamLabel: rowLabel
						name: "modItemList"
						implicitWidth: modThirdLayer.width - 2
						addItemManually: false
						headerLabels: [qsTr("Include"), "   ", qsTr("Display Plot")]
						// source: "factors"
						source: modThirdLayer.modTypeValue == "indicators" ? factors.name : (rowValue == "covariances" ? {values: concatFactorTitles(factors.factorsTitles)} : {values: factors.factorsTitles})
						rowComponent: RowLayout
						{
							Text { text: rowValue ; Layout.preferredWidth: 200*jaspTheme.uiScale }
							CheckBox { 
								name: "includeModeration";
								id: includeModeration
								checked: modSecondLayer.modInvValue == "configuralInvariance" ||
									(modSecondLayer.modInvValue == "metricInvariance" && modFourthLayer.modParamValue != "loadings") ||
										(modSecondLayer.modInvValue == "scalarInvariance" && (modFourthLayer.modParamValue != "loadings" && modFourthLayer.modParamValue != "intercepts")) ||
											(modSecondLayer.modInvValue == "strictInvariance" && (modFourthLayer.modParamValue != "loadings" && modFourthLayer.modParamValue != "intercepts" && modFourthLayer.modParamValue != "residualVariances"));
							}
						}
					}
				}
			}
		}
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

		CheckBox { label: qsTr("Show syntax")         ; name: "showSyntax" }
		
	}

	
	Section
	{
		title: qsTr("Plots")
		id: plots

		property var names: []
		// property var variablesIncluded: 
		TabView 
		{
			
			values: invOpts.firstLayerValues
			name: "plotModelList"
			id: plotFirstLayer
			optionKey: "keyValue"
			optionKeyLabel: "keyLabel"
			addItemManually: false
			rowComponent: TabView 
			{
				id: plotSecondLayer
				property string plotInvValue: rowValue 
				name: "plotTypeList"
				optionKey: "keyValue"
				optionKeyLabel: "keyLabel"
				addItemManually: false
				values: [{value: "indicators", label: qsTr("Indicators")}, {value: "factors", label: qsTr("Factors")}]

				rowComponent: TabView 
				{
					id: plotThirdLayer
					property string plotTypeValue: rowValue
					optionKey: "keyValue"
					optionKeyLabel: "keyLabel"
					name: "plotParameterList"
					addItemManually: false

					values: getValuesModOptions(plotSecondLayer.plotInvValue, rowValue, factors.factorsTitles.length)
				
					rowComponent: ComponentsList
					{
						name: "plotItemList"
						addItemManually: false
						headerLabels: [qsTr("Moderator 1"), qsTr("Moderator 2"), qsTr("Display plot")]
						rSource: "plotOptions." + plotSecondLayer.plotInvValue + "." + plotThirdLayer.plotTypeValue + "." + rowValue
						
						rowComponent: RowLayout
						{
							Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
							DropDown
							{
								name: "plotModerator1"
								id: plotMod1
								// source: [moderators]
								source: [moderators, {values: plots.names}, {name: "moderatorInteractions", condition: "includeInteraction"}]
								// source: [moderators, {name: "moderatorInteractions", condition: "includeInteraction"}]


								addEmptyValue: true
							}
							DropDown
							{
								name: "plotModerator2"
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

}


