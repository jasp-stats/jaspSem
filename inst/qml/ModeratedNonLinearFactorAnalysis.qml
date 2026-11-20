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
	// Anchor-aware defaults for generated moderation rows.
	property var defaultAnchorIndicators:
	{
		// factorsItems can notify before its child lists exist. countVariables also
		// notifies on their termsChanged, so initial assignments recompute anchors.
		if (factors.countVariables === 0)
			return [];
		return getDefaultAnchors(factors.factorsItems);
	}

	// JASP 0.98.1 keeps the values a generated row had when its list was bound and pushes them back onto the row
	// whenever the list's source is reset (a factor renamed, an indicator removed or added); a re-added indicator's new
	// row starts from them as well (observed in JASP Desktop). The form therefore remembers, per box, the value it should
	// have together with its preset default at that moment, and restores that value after such a push (checked in JASP
	// Desktop 0.98.1). Memory of this form only, not saved: a saved file keeps the values themselves.
	property var	rowMemory:		({})
	// The number of factors as an int: a rename does not change it, so the parameter lists are not rebuilt on a rename.
	property int	factorCount:	factors.factorsTitles.length

	function rememberRow(key, value, preset)
	{
		rowMemory[key] = {value: value, preset: preset};
	}

	function forgetModel(model)
	{
		for (var key in rowMemory)
			if (key.indexOf(model + "|") === 0)
				delete rowMemory[key];
	}

	// An interaction row is remembered by its unordered pair, so that "Male:Age" finds the choice made for "Age:Male".
	// The pair is found among the assigned moderators (whole names, in their order, as
	// combinePairs builds the term and R resolves it) and kept as a sorted JSON array, so names containing ":" cannot
	// collide. A term that does not resolve to exactly one pair gets no key ("") and no memory.
	function interactionMemoryKey(term, moderatorNames)
	{
		var found = [];
		for (var i = 0; i < moderatorNames.length; i++)
			for (var j = i + 1; j < moderatorNames.length; j++)
				if (moderatorNames[i] + ":" + moderatorNames[j] === term)
				{
					var key = JSON.stringify([moderatorNames[i], moderatorNames[j]].sort());
					if (found.indexOf(key) === -1)
						found.push(key);
				}
		return found.length === 1 ? "interaction|" + found[0] : "";
	}

	// JASP keeps an interaction row, and pushes its cached value, by the label; after a reorder
	// of moderators whose names contain ":", the same label can mean another pair. interactionLabels holds the key each label
	// last resolved to ("" = no pair or more than one).
	property var	interactionLabels:	({})

	// The value an interaction row should have: when it is built, after JASP pushes a value onto it, and when its label comes
	// to mean another pair. The pair's remembered choice if there is one; otherwise the row's value if its label has not meant
	// anything else before (a new or loaded row), else unticked (the value belonged to something else). A label without
	// exactly one pair keeps its value and is not remembered.
	function interactionRowValue(term, key, checked)
	{
		var previous = interactionLabels[term];
		interactionLabels[term] = key;
		if (key === "")
			return checked;
		if (rowMemory[key] === undefined)
			rememberRow(key, previous === undefined || previous === key ? checked : false, false);
		return rowMemory[key].value;
	}

	function getDefaultAnchors(factorItems)
	{
		var indicators = [];
		for (var i = 0; i < factorItems.length; i++)
			indicators.push(factorItems[i] ? factorItems[i].columnsNames : []);

		var anchors = [];
		for (var f = 0; f < indicators.length; f++)
		{
			var anchor = "";
			for (var j = 0; j < indicators[f].length; j++)
			{
				var item = indicators[f][j];
				var pure = true;
				for (var other = 0; other < indicators.length; other++)
					if (other !== f && indicators[other].indexOf(item) !== -1)
						pure = false;
				if (pure)
				{
					anchor = item;
					break;
				}
			}
			if (anchor === "")
				for (var candidate = 0; candidate < indicators[f].length; candidate++)
					if (anchors.indexOf(indicators[f][candidate]) === -1)
					{
						anchor = indicators[f][candidate];
						break;
					}
			// No third fallback when earlier factors have used all candidates; R's distinct-anchor check decides.
			anchors.push(anchor);
		}
		return anchors;
	}

	function defaultModeration(model, parameter, item, anchors)
	{
		if (model === "invarianceTestCustom")
			return false;
		if (parameter === "loadings")
			return model === "invarianceTestConfigural" && anchors.indexOf(item) === -1;
		if (parameter === "intercepts")
			return (model === "invarianceTestConfigural" || model === "invarianceTestMetric") && anchors.indexOf(item) === -1;
		if (parameter === "residualVariances")
			return model !== "invarianceTestStrict";
		return true;
	}

	// function for getting the values for the plots
	function getValuesModOptions(rwValue, factorCount) {
		if (rwValue == "indicators") {
			return [{value: "loadings", label: qsTr("Loadings")}, {value: "intercepts", label: qsTr("Intercepts")}, {value: "residualVariances", label: qsTr("Residual variances")}];
		}
		if (factorCount == 2) {
			return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}, {value: "covariances", label: qsTr("Correlations")}];
		}
		return [{value: "variances", label: qsTr("Variances")}, {value: "means", label: qsTr("Means")}];
	}

	// The correlation row is keyed by the factor names ("Factor1:Factor2"), which a
	// rename never changes (as the variance and mean rows are); it shows the titles. JASP does not rebuild a title key
	// after a rename, so a title key went stale.
	function factorPairRow(factorTitles)
	{
		var names	= [];
		var titles	= [];
		for (var i = 0; i < factorTitles.length; i++)
		{
			names.push(factorTitles[i].value);
			titles.push(factorTitles[i].label);
		}
		return [{value: names.join(":"), label: titles.join(":")}];
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
		// Scale indicators are continuous, ordinal indicators categorical.
		allowedColumns:		["scale", "ordinal"]
		keepAvailableVariables: true
		info: qsTr("Define latent factors and assign observed indicator variables to each factor. Scale variables are continuous indicators; ordinal variables are categorical (binary or ordinal) indicators.")
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
				info: qsTr("Variables that may moderate the parameters of the measurement model. A scale moderator with two distinct values is used as coded (effects per unit); with more values it is standardized on the analysed cases (effects per standard deviation) and can also enter as a square and a cube. A nominal moderator is dummy coded against its first level. Cases with a missing moderator value are excluded from all models.")
				rowComponentTitle: qsTr("Square   Cubic")
				rowComponent: RowLayout
				{
					CheckBox
					{
						name: "moderatorSquaredEffect"
						id: squared
						visible: moderators.getVariableType(rowValue) === columnTypeScale
						info: qsTr("Include a quadratic (squared) effect for this moderator. Used only for continuous moderators (more than two distinct values).")
					}
					CheckBox
					{
						name: "moderatorCubicEffect"
						id: cubic
						visible: moderators.getVariableType(rowValue) === columnTypeScale
						info: qsTr("Include a cubic effect for this moderator. Used only for continuous moderators (more than two distinct values).")
					}
				}
			}
			CheckBox
			{
				id: 						syncAnalysisBox
				name: 					"syncAnalysisBox"
				label: 					qsTr("<b>Start/Sync Analysis</b>")
				checked: 				false
				info: qsTr("Fit the models. While unchecked, nothing is fitted; results of the last synchronized run stay visible as long as the model settings are unchanged.")
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
				info: qsTr("Include interaction terms between pairs of moderators. An included interaction stays included when you reorder the moderators; its name follows their order.")
				rowComponent: RowLayout
				{
					Text { text: rowValue; Layout.preferredWidth: 150 * jaspTheme.uiScale }
					CheckBox
					{
						name:	"moderatorInteractionTermsInclude"
						id:		includeInteraction
						// A reorder of the moderators renames the term ("Age:Male" -> "Male:Age") and
						// JASP builds a new row; the choice is remembered by the unordered pair and survives JASP's pushes. A kept row
						// whose label comes to mean another pair takes that pair's choice once JASP has updated it.
						property string	memoryKey:	interactionMemoryKey(rowValue, moderators.columnsNames)
						property bool	settled:	false
						function reconcile()
						{
							var wanted = interactionRowValue(rowValue, memoryKey, checked);
							if (checked !== wanted)
								checked = wanted;
						}
						function settle()
						{
							settled = true;
							reconcile();
						}
						Component.onCompleted:	Qt.callLater(settle)
						onMemoryKeyChanged:		if (settled) Qt.callLater(reconcile)
						onClicked:				if (memoryKey !== "") rememberRow(memoryKey, checked, false)
						onCheckedChanged:		if (settled && memoryKey !== "") { if (control.activeFocus) rememberRow(memoryKey, checked, false); else Qt.callLater(reconcile) }
					}
				}
			}
		}
	}

	Section
	{
		id: invOpts
		title: qsTr("Moderation Options")
		info: qsTr("Choose the models to fit (invariance presets) and, for each model, which parameters depend on the moderators.")

		// property var names: []

		Group
		{
			columns: 1
			title: qsTr("Invariance Tests")
			info: qsTr("Select the models to fit. Each model starts from a preset of moderation settings (below), which you can change. The presets are nested: every parameter that a model leaves unmoderated is also unmoderated in the models below it. Each factor's anchor indicator keeps its loading and intercept (or thresholds) unmoderated in every preset.")
			CheckBox { name: "invarianceTestConfigural" ; checked: false ; label: qsTr("Configural"); id: invarianceTestConfigural; info: qsTr("Preset: loadings and intercepts (thresholds) of all indicators except the anchors, residual variances, factor variances, factor means and, with two factors, the factor correlation depend on the moderators.") }
			CheckBox { name: "invarianceTestMetric" ; checked: false ; label: qsTr("Metric"); id: invarianceTestMetric; info: qsTr("Preset: as Configural, but no loading depends on the moderators.") }
			CheckBox { name: "invarianceTestScalar" ; checked: false ; label: qsTr("Scalar"); id: invarianceTestScalar; info: qsTr("Preset: as Metric, but no intercept or threshold depends on the moderators.") }
			CheckBox { name: "invarianceTestStrict" ; checked: false ; label: qsTr("Strict"); id: invarianceTestStrict; info: qsTr("Preset: as Scalar, but no residual variance depends on the moderators. With only ordinal indicators, Scalar and Strict are the same model.") }
			CheckBox { name: "invarianceTestCustom" ; checked: false;  label: qsTr("Custom"); id: invarianceTestCustom; info: qsTr("Preset: nothing depends on the moderators; tick the parameters that should.") }
		}

		property var firstLayerValues: [invarianceTestConfigural, invarianceTestMetric, invarianceTestScalar, invarianceTestStrict, invarianceTestCustom].filter(x => x.checked).map(x => ({value: x.name, label: x.label}))
		// A model switched off forgets its remembered boxes, so switching it on again builds its list as before.
		onFirstLayerValuesChanged:
		{
			var enabled = firstLayerValues.map(x => x.value);
			for (const model of ["invarianceTestConfigural", "invarianceTestMetric", "invarianceTestScalar", "invarianceTestStrict", "invarianceTestCustom"])
				if (enabled.indexOf(model) === -1)
					forgetModel(model);
		}

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
			info: qsTr("For each model, tick the parameters that depend on the moderators; a ticked parameter depends on all moderator terms. The default anchor of a factor is its first indicator that loads on this factor only, otherwise its first indicator that is not already another factor's anchor; its loading and intercept are unticked by default. When a factor's default anchor changes (for example, its first indicator is removed), the loading and intercept boxes of the old and the new anchor return to their defaults. All other boxes keep their values when you rename a factor, add, remove or reorder indicators, or reorder the moderators; an indicator that you assign again gets back its earlier values. Residual variances exist only for continuous (scale) indicators. The factor correlation can depend on the moderators only with exactly two factors.")
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
					values: getValuesModOptions(rowValue, factorCount)
					rowComponent: ComponentsList
					{
						id: modFourthLayer
						property string moderationParamValue: rowValue
						property string moderationParamLabel: rowLabel
						name: "moderationItemList"
						implicitWidth: modThirdLayer.width - 2
						addItemManually: false
						headerLabels: [qsTr("Include"), "   ", qsTr("Display Plot")]
						// Categorical indicators have no residual-variance rows.
						source: modThirdLayer.moderationTypeValue == "indicators" ?
							{name: factors.name, use: rowValue == "residualVariances" ? "type=scale" : ""} :
							(rowValue == "covariances" ? {values: factorPairRow(factors.factorsTitles)} : {values: factors.factorsTitles})
						rowComponent: RowLayout
						{
							Text { text: rowLabel ; Layout.preferredWidth: 200*jaspTheme.uiScale }
							CheckBox {
								name: "includeIndividualModeration";
								id: includeIndividualModeration
								info: modFourthLayer.moderationParamValue == "covariances" ? qsTr("Allow the Fisher z-transformed factor correlation to depend on the moderators. When unchecked, the correlation is constant, but the covariance can still vary with the factor variances.") : ""
								// Anchored, nested preset defaults (strict within scalar within metric within configural; custom: none ticked).
								// Anchor changes must not reapply factor or residual defaults.
								// A new anchor array must not reapply an unchanged row default.
								property bool presetDefaultChecked: defaultModeration(modSecondLayer.moderationInvValue,
									modFourthLayer.moderationParamValue, rowValue,
									(modFourthLayer.moderationParamValue === "loadings" ||
									 modFourthLayer.moderationParamValue === "intercepts") ? defaultAnchorIndicators : [])
								checked: presetDefaultChecked
								// The value this box should have is remembered (rowMemory) and restored after
								// JASP pushes cached values onto the row. When the box's preset default changes (the factor's anchor
								// moved: only the loading and intercept boxes of the old and the new anchor), the box takes the new
								// default; every other box keeps its value. A rebuilt row (an indicator assigned again) gets back
								// its last value, or its preset if that changed in the meantime.
								property string	memoryKey:	modSecondLayer.moderationInvValue + "|" + modFourthLayer.moderationParamValue + "|" + rowValue
								property bool	settled:	false
								function settle()
								{
									var m = rowMemory[memoryKey];
									if (m === undefined)
										rememberRow(memoryKey, checked, presetDefaultChecked);
									else
									{
										var wanted = m.preset === presetDefaultChecked ? m.value : presetDefaultChecked;
										rememberRow(memoryKey, wanted, presetDefaultChecked);
										if (checked !== wanted)
											checked = wanted;
									}
									settled = true;
								}
								function restore()
								{
									var m = rowMemory[memoryKey];
									if (m !== undefined && checked !== m.value)
										checked = m.value;
								}
								Component.onCompleted:			Qt.callLater(settle)
								onClicked:						rememberRow(memoryKey, checked, presetDefaultChecked)
								onCheckedChanged:				if (settled) { if (control.activeFocus) rememberRow(memoryKey, checked, presetDefaultChecked); else Qt.callLater(restore) }
								onPresetDefaultCheckedChanged:	if (settled) { rememberRow(memoryKey, presetDefaultChecked, presetDefaultChecked); Qt.callLater(restore) }
							}
						}
					}
				}
			}
		}
	}


	Section
	{
		title: qsTr("Estimation Options")

		// Three choices from the same fit; never the word "robust" in a label.
		DropDown
		{
			name: "errorCalculationMethod"
			label: qsTr("Standard errors")
			startValue: "robustHuberWhite"
			values: [
				{value: "robustHuberWhite", label: qsTr("Huber-White sandwich")},
				{value: "robustHuberWhiteReducedInformation", label: qsTr("Huber-White sandwich, reduced information")},
				{value: "standard", label: qsTr("Standard")}
			]
			info: qsTr("Standard errors for the parameter tables, the likelihood-ratio tests and the plot bands; all three come from the same fit, so changing them does not refit the models. Huber-White sandwich (default) uses the exact information matrix. The reduced-information variant follows the convention of other MNLFA software; if its information matrix is not positive definite for a model, that model is shown without standard errors. Standard uses the inverse observed information and the ordinary likelihood-ratio test; the sandwich choices use scaled tests. Sandwich standard errors are valid for the model as fitted (its pseudo-true parameters); they do not correct bias from a wrong response model, invalid anchors or unmodelled dependence. Their calibration was studied only for ordinal, binary, continuous and mixed indicators, one or two factors, logit and probit links, and sample sizes of 500 to 2000.")
		}

		DropDown
		{
			name: "categoricalIndicatorLink"
			label: qsTr("Categorical indicator link")
			values: [{value: "logit", label: qsTr("Logit")}, {value: "probit", label: qsTr("Probit")}]
			startValue: "logit"
			// Read the types from the per-factor lists. The FactorsForm's own columnsTypes is not set while
			// a saved analysis is initialized (FactorsFormBase::termsChangedHandler returns early), which left
			// the control disabled after reopening a file with ordinal indicators.
			enabled: factors.countVariables > 0 && factors.factorsItems.some(function(item) {
				return item && item.columnsTypes && item.columnsTypes.indexOf("ordinal") !== -1;
			})
			info: qsTr("Link function of the ordinal and binary indicators (indicators assigned as ordinal columns): logit (default) or probit. Available only when an ordinal indicator is assigned; continuous indicators are not affected.")
		}

		RadioButtonGroup
		{
			name:  "indicatorPreprocessing"
			title: qsTr("Indicator preprocessing")
			info:  qsTr("Transformation of the continuous (scale) indicators before fitting, with the mean and standard deviation of the analysed cases. Ordinal indicators are never transformed.")

			RadioButton { value: "none";        label: qsTr("None");           checked: true;	info: qsTr("Use the continuous indicators as they are.")							}
			RadioButton { value: "center";      label: qsTr("Center");							info: qsTr("Subtract the mean from each continuous indicator.")						}
			RadioButton { value: "standardize"; label: qsTr("Z-standardize");					info: qsTr("Subtract the mean and divide by the standard deviation of each continuous indicator.")	}
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
			// The confidence level is 1 - alpha, so alpha must lie strictly between 0 and 1.
			DoubleField
			{
				name: "parameterEstimatesAlphaLevel"
				label: qsTr("Significance level")
				negativeValues: false
				min: 0
				max: 1
				inclusive: JASP.None
				decimals: 4
				defaultValue: 0.05
				info: qsTr("Significance level alpha of the tests; confidence intervals and plot bands have level 1 - alpha.")
			}
			CheckBox { name: "parameterEstimatesLoadings"; label: qsTr("Loadings"); info: qsTr("Display the loadings: baseline and moderation effects.") }
			CheckBox { name: "parameterEstimatesIntercepts"; label: qsTr("Intercepts"); info: qsTr("Display the intercepts of continuous indicators and the thresholds of ordinal indicators, with their moderation effects.") }
			CheckBox { name: "parameterEstimatesResidualVariances"; label: qsTr("Residual variances"); info: qsTr("Display the residual variances of continuous indicators; moderation effects are shown as variance ratios.") }
			CheckBox { name: "parameterEstimatesFactorVariance"; label: qsTr("Factor variances"); info: qsTr("Display the moderation effects on the factor variances as variance ratios; the baseline variance is fixed at 1.") }
			CheckBox { name: "parameterEstimatesFactorMeans"; label: qsTr("Factor means"); info: qsTr("Display the moderation effects on the factor means; the baseline mean is fixed at 0.") }
			CheckBox { name: "parameterEstimatesFactorCovariances"; label: qsTr("Factor correlations"); info: qsTr("Display the factor correlations: the baseline on the Fisher z and the correlation scale, moderation effects on the Fisher z scale.") }

		}

		Group
		{
			CheckBox { label: qsTr("Show syntax")   ; name: "showSyntax"; info: qsTr("Show R code that fits each model with the MoFIA package outside JASP.") }
			CheckBox { label: qsTr("Show warnings") ; name: "warnings"  ; info: qsTr("List the warnings of each model fit. Identification, convergence and quadrature notes are always shown.") }
		}

		// EAP scores of the selected model are saved as columns owned by the analysis.
		CheckBox
		{
			name: "factorScoreSavedToData"
			label: qsTr("Save factor scores to data")
			checked: false
			info: qsTr("Save expected a posteriori factor scores from the selected model. Scores include moderated factor means and variances; posterior standard deviations do not include parameter uncertainty.")

			DropDown
			{
				name: "factorScoreModel"
				label: qsTr("Model")
				// Register the source dependency before restoring a saved model choice.
				source: modFirstLayer
				info: qsTr("Select one of the enabled invariance models for the factor scores. Scores are saved only from a model that converged; otherwise a footnote under the table 'Factor Scores' says why.")
			}

			TextField
			{
				name: "factorScoreColumnPrefix"
				label: qsTr("Column name prefix")
				// Column name = prefix + factor title, default prefix "FS_".
				defaultValue: "FS_"
				info: qsTr("Text placed before each factor title in the saved score column names.")
			}
		}

	}


	Section
	{
		title: qsTr("Plots")
		id: plots
		info: qsTr("Plot how a moderated parameter changes with the moderators, with pointwise confidence bands. For each model and parameter, the rows list the indicators or factors whose parameter depends on the moderators.")

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

					values: getValuesModOptions(rowValue, factorCount)

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
								info: qsTr("Moderator on the x-axis. An included interaction term A:B puts A on the x-axis with lines for B; then leave Moderator 2 empty.")
							}
							DropDown
							{
								name: "plotModerator2"
								id: plotMod2
								addEmptyValue: true
								source: plotMod1
								// source: [{id: plotMod1, discard: {values: [plotMod1.currentValue]}}]
								info: qsTr("Optional second moderator: lines for its values or levels (continuous: mean - 1 SD, mean, mean + 1 SD). Other moderators are held at their mean, 0 or reference level.")
							}

							CheckBox { name: "includePlot"; enabled: plotMod1.currentValue !== "" || plotMod2.currentValue !== ""; info: qsTr("Draw this plot.") }
						}
					}
				}
			}
		}
	}

}


