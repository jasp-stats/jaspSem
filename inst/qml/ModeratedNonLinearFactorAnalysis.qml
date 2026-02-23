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
	function getValuesModOptions(rwValue, factorCount) {
		if (rwValue == "indicators") {
			return [{value: "loadings", label: qsTr("Loadings")}, {value: "intercepts", label: qsTr("Intercepts")}, {value: "residualVariances", label: qsTr("Residual variances")}];
		}
		if (factorCount == 2) {
			return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}, {value: "covariances", label: qsTr("Covariances")}];
		}
		return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}];
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
		info: qsTr("Define latent factors and assign observed indicator variables to each factor.")
	}

	Section
	{
		title: qsTr("Moderation")
		expanded: true
		id: mod
		info: qsTr("Specify moderator variables and their effects on measurement model parameters.")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight * 0.65
			AvailableVariablesList
			{
				name: "availableModerators"
				source: [{ isDataSetVariables: true, discard: factors.name }]
			}
			AssignedVariablesList
			{
				id: moderators
				allowedColumns:		["scale", "nominal"]
				title: qsTr("Moderators")
				name: "moderators"
				info: qsTr("Variables hypothesized to moderate the measurement model parameters. Continuous moderators can optionally include squared and cubic effects.")
				rowComponentTitle: qsTr("Square   Cubic")
				rowComponent: RowLayout
				{
					CheckBox
					{
						name: "moderatorSquaredEffect"
						id: squared
						visible: moderators.getVariableType(rowValue) === columnTypeScale
						info: qsTr("Include a quadratic (squared) effect for this moderator.")
					}
					CheckBox
					{
						name: "moderatorCubicEffect"
						id: cubic
						visible: moderators.getVariableType(rowValue) === columnTypeScale
						info: qsTr("Include a cubic effect for this moderator.")
					}
				}
			}
			CheckBox
			{
				id: 						syncAnalysisBox
				name: 					"syncAnalysisBox"
				label: 					qsTr("<b>Start/Sync Analysis</b>")
				checked: 				false
				info: qsTr("Click to start or synchronize the analysis. The analysis does not run automatically due to computational intensity.")
				Component.onCompleted:
				{
						background.color = "#ffcb98"
				}
			}
		}

		Group
		{
			title: qsTr("Assumption Check")
			info: qsTr("Check measurement invariance assumptions by testing model fit across groups defined by the moderator variables.")
			CheckBox
			{
				name: "checkModelFitPerGroup";
				checked: false ;
				label: qsTr("Check model fit per group");
				id: fitPerGroup;
				enabled: moderators.columnsNames != ""
				info: qsTr("Fit the measurement model separately in each group to verify that the model fits adequately before testing moderation.")
			}
			IntegerField
			{
				name: "splitContinuousVariablesIntoGroups";
				label: qsTr("Split continuous variables into groups:");
				defaultValue: 2;
				enabled: fitPerGroup.checked;
				min: 2
				info: qsTr("Number of groups into which continuous moderator variables are split for the assumption check.")
			}
			CheckBox { name: "addGroupVariableToData"; label: qsTr("Add group variable to data"); enabled: fitPerGroup.checked; info: qsTr("Save the group variable created by splitting the continuous moderator to the dataset.") }
		}

		ColumnLayout
		{
			Layout.alignment: Qt.AlignRight

			ComponentsList
			{
				visible: moderators.columnsNames.length > 1
				preferredWidth: form.width * 0.4
				title: qsTr("Interaction Terms")
				name: "moderatorInteractionTerms"
				id: interactions
				addItemManually: false
				values: combinePairs(moderators.columnsNames)
				headerLabels: [qsTr("Include")]
				info: qsTr("Include interaction terms between pairs of moderators.")
				rowComponent: RowLayout
				{
					Text { text: rowValue; Layout.preferredWidth: 150 * jaspTheme.uiScale }
					CheckBox { name: "moderatorInteractionTermsInclude"; id: includeInteraction }
				}
			}
		}
	}

	Section
	{
		id: invOpts
		title: qsTr("Moderation Options")
		info: qsTr("Configure invariance test levels and specify which individual parameters should be tested for moderation.")

		// property var names: []

		Group
		{
			columns: 1
			title: qsTr("Invariance Tests")
			info: qsTr("Select the levels of measurement invariance to test. Each level constrains an additional set of parameters to be equal across moderator values.")
			CheckBox { name: "invarianceTestConfigural" ; checked: false ; label: qsTr("Configural"); id: invarianceTestConfigural; info: qsTr("Test configural invariance: same factor structure across groups but all parameters free.") }
			CheckBox { name: "invarianceTestMetric" ; checked: false ; label: qsTr("Metric"); id: invarianceTestMetric; info: qsTr("Test metric invariance: constrain factor loadings to be equal across groups.") }
			CheckBox { name: "invarianceTestScalar" ; checked: false ; label: qsTr("Scalar"); id: invarianceTestScalar; info: qsTr("Test scalar invariance: constrain factor loadings and intercepts to be equal across groups.") }
			CheckBox { name: "invarianceTestStrict" ; checked: false ; label: qsTr("Strict"); id: invarianceTestStrict; info: qsTr("Test strict invariance: constrain factor loadings, intercepts, and residual variances to be equal across groups.") }
			CheckBox { name: "invarianceTestCustom" ; checked: false;  label: qsTr("Custom"); id: invarianceTestCustom; info: qsTr("Define a custom set of parameter constraints for invariance testing.") }
		}

		property var firstLayerValues: [invarianceTestConfigural, invarianceTestMetric, invarianceTestScalar, invarianceTestStrict, invarianceTestCustom].filter(x => x.checked).map(x => ({value: x.name, label: x.label}))

		TabView
		{
			Layout.columnSpan: 1
			preferredWidth: 100
			Layout.fillWidth: true
			values: invOpts.firstLayerValues
			title: qsTr("Include Individual Moderations")
			name: "includeIndividualModerationsList"
			optionKey: "keyValue"
			optionKeyLabel: "keyLabel"
			id: modFirstLayer
			addItemManually: false
			info: qsTr("For each invariance level, specify which individual measurement parameters should be tested for moderation effects.")
			rowComponent: TabView
			{
				id: modSecondLayer
				property string moderationInvValue: rowValue
				property string moderationInvLabel: rowLabel
				name: "moderationTypeList"
				addItemManually: false
				values: [{value: "indicators", label: qsTr("Indicators")}, {value: "factors", label: qsTr("Factors")}]
				optionKey: "keyValue"
				optionKeyLabel: "keyLabel"
				rowComponent: TabView
				{
					id: modThirdLayer
					property string moderationTypeValue: rowValue
					property string moderationTypeLabel: rowLabel
					name: "moderationParameterList"
					addItemManually: false
					optionKey: "keyValue"
					optionKeyLabel: "keyLabel"
					values: getValuesModOptions(rowValue, factors.factorsTitles.length)
					rowComponent: ComponentsList
					{
						id: modFourthLayer
						property string moderationParamValue: rowValue
						property string moderationParamLabel: rowLabel
						name: "moderationItemList"
						implicitWidth: modThirdLayer.width - 2
						addItemManually: false
						headerLabels: [qsTr("Include"), "   ", qsTr("Display Plot")]
						source: modThirdLayer.moderationTypeValue == "indicators" ? factors.name : (rowValue == "covariances" ? {values: concatFactorTitles(factors.factorsTitles)} : {values: factors.factorsTitles})
						rowComponent: RowLayout
						{
							Text { text: rowLabel ; Layout.preferredWidth: 200*jaspTheme.uiScale }
							CheckBox {
								name: "includeIndividualModeration";
								id: includeIndividualModeration
								checked: 
								(modSecondLayer.moderationInvValue == "invarianceTestConfigural" && modFourthLayer.moderationParamValue != "variances" && modFourthLayer.moderationParamValue != "means") ||
									(modSecondLayer.moderationInvValue == "invarianceTestMetric" && modFourthLayer.moderationParamValue != "loadings" && modFourthLayer.moderationParamValue != "means") ||
										(modSecondLayer.moderationInvValue == "invarianceTestScalar" && (modFourthLayer.moderationParamValue != "loadings" && modFourthLayer.moderationParamValue != "intercepts")) ||
											(modSecondLayer.moderationInvValue == "invarianceTestStrict" && (modFourthLayer.moderationParamValue != "loadings" && modFourthLayer.moderationParamValue != "intercepts" && modFourthLayer.moderationParamValue != "residualVariances"));
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
		info: qsTr("Select additional output tables and parameter estimates to display.")

		Group
		{
			title: qsTr("Parameter Estimates")
			info: qsTr("Select which measurement model parameter estimates to display in the output.")
			DoubleField
			{
				name: "parameterEstimatesAlphaLevel"
				label: qsTr("Significance level")
				negativeValues: false
				decimals: 4
				defaultValue: 0.05
				info: qsTr("Significance level for flagging parameter estimates.")
			}
			CheckBox { name: "parameterEstimatesLoadings"; label: qsTr("Loadings"); info: qsTr("Display factor loading estimates.") }
			CheckBox { name: "parameterEstimatesIntercepts"; label: qsTr("Intercepts"); info: qsTr("Display intercept estimates for observed indicators.") }
			CheckBox { name: "parameterEstimatesResidualVariances"; label: qsTr("Residual variances"); info: qsTr("Display residual variance estimates for observed indicators.") }
			CheckBox { name: "parameterEstimatesFactorVariance"; label: qsTr("Factor variances"); info: qsTr("Display latent factor variance estimates.") }
			CheckBox { name: "parameterEstimatesFactorMeans"; label: qsTr("Factor means"); info: qsTr("Display latent factor mean estimates.") }
			CheckBox { name: "parameterEstimatesFactorCovariances"; label: qsTr("Factor covariances"); info: qsTr("Display latent factor covariance estimates.") }

		}

		CheckBox { label: qsTr("Show syntax")         ; name: "showSyntax"; info: qsTr("Display the lavaan syntax used to estimate the model.") }

	}


	Section
	{
		title: qsTr("Plots")
		id: plots
		info: qsTr("Configure marginal effects plots showing how measurement parameters change as a function of the moderator variables.")

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

					values: getValuesModOptions(rowValue, factors.factorsTitles.length)

					rowComponent: ComponentsList
					{
						name: "plotItemList"
						addItemManually: false
						headerLabels: [qsTr("Moderator 1"), qsTr("Moderator 2"), qsTr("Display plot")]
						rSource: "plotOptionsForQml." + plotSecondLayer.plotInvValue + "." + plotThirdLayer.plotTypeValue + "." + rowValue

						rowComponent: RowLayout
						{
							Text { text: rowValue ; Layout.preferredWidth: 150*jaspTheme.uiScale }
							DropDown
							{
								name: "plotModerator1"
								id: plotMod1
								// Squared/cubic effects are automatically included in marginal effect plots,
								// so we only show base moderators and interaction terms
								source: [moderators, {name: "moderatorInteractionTerms", condition: "moderatorInteractionTermsInclude"}]

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


