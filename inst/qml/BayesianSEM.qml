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

	TabView
	{
		id: models
		name: "models"
		maximumItems: 9
		newItemName: qsTr("Model 1")
		optionKey: "name"
		info: qsTr("Specify one or more structural equation models using lavaan syntax. Each tab represents a separate model.")

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan; showLineNumber: true; info: qsTr("Enter the model specification in lavaan syntax.") }
	}

	RadioButtonGroup
	{
		title: qsTr("Data")
		name: "dataType"
		columns: 2
		info: qsTr("Select the type of data to be used in the analysis.")
		RadioButton { value: "raw"; label: qsTr("Raw"); checked: true; info: qsTr("Use raw data for the analysis.") }
	}

	Section
	{
		title: qsTr("Model Options")
		info: qsTr("Options for configuring the structural equation model.")
		Group
		{
			DropDown
			{
				name: "factorScaling"
				label: qsTr("Factor scaling")
				info: qsTr("Method used to set the metric of latent variables.")
				values:
				[
					{ label: qsTr("Factor loadings"),	value: "factorLoading"	},
					{ label: qsTr("Factor variance"),	value: "factorVariance"	},
					{ label: qsTr("None"),				value: "none"			}
				]
			}

			CheckBox
			{
				name: "meanStructure"
				label: qsTr("Mean structure")
				checked: false
				info: qsTr("Model the means of observed and latent variables in addition to the covariance structure.")
				CheckBox { name: "latentInterceptFixedToZero";		label: qsTr("Latent intercepts fixed to zero");		checked: true;	info: qsTr("Fix the intercepts of latent variables to zero.") }
				CheckBox { name: "manifestInterceptFixedToZero";	label: qsTr("Manifest intercepts fixed to zero");	checked: false;	info: qsTr("Fix the intercepts of observed variables to zero.") }
			}

			CheckBox { name: "orthogonal"; label: qsTr("Assume factors uncorrelated"); checked: false; info: qsTr("Assume that all latent factors are uncorrelated.") }
		}
	}

	Section
	{
		title: qsTr("MCMC Options")
		info: qsTr("Settings for the Markov chain Monte Carlo (MCMC) sampling algorithm.")
		columns: 2

		Group
		{
			title: qsTr("Sampling")
			
			IntegerField
			{
				name: "mcmcBurnin"
				label: qsTr("Burn-in")
				defaultValue: 500
				min: 100
				max: 10000
				info: qsTr("Number of initial MCMC iterations discarded before collecting samples.")
			}

			IntegerField
			{
				name: "mcmcSamples"
				label: qsTr("Samples")
				defaultValue: 1000
				min: 100
				max: 100000
				info: qsTr("Number of MCMC samples to draw after the burn-in period.")
			}

			IntegerField
			{
				name: "mcmcChains"
				label: qsTr("Chains")
				defaultValue: 3
				min: 1
				max: 10
				info: qsTr("Number of independent MCMC chains to run.")
			}

			IntegerField
			{
				name: "mcmcThin"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: 100
				info: qsTr("Keep every nth sample to reduce autocorrelation in the chains.")
			}
		}

		Group
		{
			title: qsTr("Other")
			SetSeed {}
		}

		Group
		{
			title: qsTr("Diagnostics")
			CheckBox { name: "convergenceDiagnostics"; label: qsTr("Convergence diagnostics (Rhat, ESS)"); info: qsTr("Add Rhat and effective sample size columns to parameter estimate tables.") }
			CheckBox
			{
				name: "tracePlots"
				label: qsTr("Traceplots")
				info: qsTr("Display traceplots of MCMC chains for model parameters.")
				DropDown
				{
					name: "tracePlotsType"
					label: qsTr("Parameter type")
					info: qsTr("Select which parameter type to display traceplots for.")
					values:
					[
						{ label: qsTr("All"),						value: "all"			},
						{ label: qsTr("Factor loadings"),			value: "loadings"		},
						{ label: qsTr("Regression coefficients"),	value: "regressions"	},
						{ label: qsTr("Variances"),					value: "variances"		},
						{ label: qsTr("Covariances"),				value: "covariances"	},
						{ label: qsTr("Intercepts"),				value: "intercepts"		}
					]
				}
			}
		}
	}

	Section
	{
		title: qsTr("Prior Options")
		info: qsTr("Configure prior distributions for model parameters.")


		Group
		{
			Layout.rowSpan: 2
			title: qsTr("Location Parameters \u2014 Normal(mean, sd)")
			info: qsTr("Normal prior distributions for location parameters.")
			columns: 3

			Text { text: qsTr("Loadings (\u03BB)") }
			DoubleField { name: "priorLoadingParam1";			label: qsTr("mean");	defaultValue: 0;	min: -1000;	max: 1000;	decimals: 2;	info: qsTr("Mean of the normal prior for factor loadings.") }
			DoubleField { name: "priorLoadingParam2";			label: qsTr("sd");		defaultValue: 10;	min: 0;		max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Standard deviation of the normal prior for factor loadings.") }

			Text { text: qsTr("Regressions (\u03B2)") }
			DoubleField { name: "priorRegressionParam1";		label: qsTr("mean");	defaultValue: 0;	min: -1000;	max: 1000;	decimals: 2;	info: qsTr("Mean of the normal prior for regression coefficients.") }
			DoubleField { name: "priorRegressionParam2";		label: qsTr("sd");		defaultValue: 10;	min: 0;		max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Standard deviation of the normal prior for regression coefficients.") }

			Text { text: qsTr("Obs. intercepts (\u03BD)") }
			DoubleField { name: "priorObservedInterceptParam1";	label: qsTr("mean");	defaultValue: 0;	min: -1000;	max: 1000;	decimals: 2;	info: qsTr("Mean of the normal prior for observed variable intercepts.") }
			DoubleField { name: "priorObservedInterceptParam2";	label: qsTr("sd");		defaultValue: 32;	min: 0;		max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Standard deviation of the normal prior for observed variable intercepts.") }

			Text { text: qsTr("Lat. intercepts (\u03B1)") }
			DoubleField { name: "priorLatentInterceptParam1";	label: qsTr("mean");	defaultValue: 0;	min: -1000;	max: 1000;	decimals: 2;	info: qsTr("Mean of the normal prior for latent variable intercepts.") }
			DoubleField { name: "priorLatentInterceptParam2";	label: qsTr("sd");		defaultValue: 10;	min: 0;		max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Standard deviation of the normal prior for latent variable intercepts.") }

			Text { text: qsTr("Thresholds (\u03C4)") }
			DoubleField { name: "priorThresholdParam1";			label: qsTr("mean");	defaultValue: 0;	min: -1000;	max: 1000;	decimals: 2;	info: qsTr("Mean of the normal prior for thresholds.") }
			DoubleField { name: "priorThresholdParam2";			label: qsTr("sd");		defaultValue: 1.5;	min: 0;		max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Standard deviation of the normal prior for thresholds.") }
		}

		Group
		{
			title: qsTr("Scale Parameters \u2014 Gamma(shape, rate) on SD")
			info: qsTr("Gamma prior distributions on the standard deviation scale.")
			columns: 3

			Text { text: qsTr("Residual SD (\u03B8)") }
			DoubleField { name: "priorResidualSdParam1";	label: qsTr("shape");	defaultValue: 1;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Shape parameter of the gamma prior for residual standard deviations.") }
			DoubleField { name: "priorResidualSdParam2";	label: qsTr("rate");	defaultValue: 0.5;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Rate parameter of the gamma prior for residual standard deviations.") }

			Text { text: qsTr("Latent SD (\u03C8)") }
			DoubleField { name: "priorLatentSdParam1";		label: qsTr("shape");	defaultValue: 1;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Shape parameter of the gamma prior for latent variable standard deviations.") }
			DoubleField { name: "priorLatentSdParam2";		label: qsTr("rate");	defaultValue: 0.5;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Rate parameter of the gamma prior for latent variable standard deviations.") }
		}

		Group
		{
			title: qsTr("Correlation Parameters \u2014 Beta(\u03B1, \u03B2)")
			info: qsTr("Beta prior distribution for correlation parameters.")
			columns: 3

			Text { text: qsTr("Correlations (\u03C1)") }
			DoubleField { name: "priorCorrelationParam1";	label: qsTr("\u03B1");	defaultValue: 1;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Alpha parameter of the beta prior for correlations.") }
			DoubleField { name: "priorCorrelationParam2";	label: qsTr("\u03B2");	defaultValue: 1;	min: 0;	max: 1000;	decimals: 2;	inclusive: JASP.None;	info: qsTr("Beta parameter of the beta prior for correlations.") }
		}
	}

	Section
	{
		title: qsTr("Output Options")
		info: qsTr("Options for additional output in the results.")

		Group
		{
			CheckBox { name: "posteriorPredictivePvalue";	label: qsTr("Posterior predictive p-value");	info: qsTr("Compute the posterior predictive p-value (PPP) using the chi-square discrepancy measure to assess global model fit.") }
			CheckBox { name: "additionalFitMeasures";		label: qsTr("Additional fit measures");			info: qsTr("Display additional Bayesian model fit measures.") }
			CheckBox { name: "warnings";					label: qsTr("Show warnings"); checked: false;	info: qsTr("Display any warnings generated during the analysis.") }

			CIField
			{
				text: qsTr("Credible interval")
				name: "ciLevel"
				info: qsTr("Width of the Bayesian credible interval for parameter estimates.")
			}
		}
	}

	Section
	{
		title: qsTr("Multigroup")
		info: qsTr("Options for multigroup Bayesian SEM.")

		Group
		{
			DropDown
			{
				id: grpvar
				name: "group"
				label: qsTr("Grouping Variable")
				showVariableTypeIcon: true
				addEmptyValue: true
				info: qsTr("Optional variable to split the analysis into groups for multigroup BSEM.")
			}
			Group
			{
				id:      constraints
				visible: grpvar.value != ""
				title: qsTr("Equality Constraints")
				info: qsTr("Constrain selected parameter types to be equal across groups.")
				CheckBox { id: eq_loadings;            name: "equalLoading";            label: qsTr("Loadings");             info: qsTr("Constrain factor loadings to be equal across groups.") }
				CheckBox { id: eq_intercepts;          name: "equalIntercept";          label: qsTr("Intercepts");           info: qsTr("Constrain intercepts to be equal across groups.") }
				CheckBox { id: eq_residuals;           name: "equalResidual";           label: qsTr("Residuals");            info: qsTr("Constrain residual variances to be equal across groups.") }
				CheckBox { id: eq_residualcovariances; name: "equalResidualCovariance"; label: qsTr("Residual covariances"); info: qsTr("Constrain residual covariances to be equal across groups.") }
				CheckBox { id: eq_means;               name: "equalMean";               label: qsTr("Means");                info: qsTr("Constrain means to be equal across groups.") }
				CheckBox { id: eq_thresholds;          name: "equalThreshold";          label: qsTr("Thresholds");           info: qsTr("Constrain thresholds to be equal across groups.") }
				CheckBox { id: eq_regressions;         name: "equalRegression";         label: qsTr("Regressions");          info: qsTr("Constrain regression coefficients to be equal across groups.") }
				CheckBox { id: eq_variances;           name: "equalLatentVariance";     label: qsTr("Latent variances");     info: qsTr("Constrain latent variances to be equal across groups.") }
				CheckBox { id: eq_lvcovariances;       name: "equalLatentCovariance";   label: qsTr("Latent covariances");   info: qsTr("Constrain latent covariances to be equal across groups.") }
			}
		}
		TextArea
		{
			name:     "freeParameters"
			title:    qsTr("Release constraints (one per line)")
			width:    250
			height:   constraints.height + grpvar.height
			textType: JASP.TextTypeLavaan
			info:     qsTr("Specify parameters to release from equality constraints using lavaan syntax, one per line.")
			visible:  eq_loadings.checked || eq_intercepts.checked || eq_residuals.checked ||
								eq_residualcovariances.checked || eq_means.checked || eq_thresholds.checked ||
								eq_regressions.checked || eq_variances.checked || eq_lvcovariances.checked
		}
	}
}
