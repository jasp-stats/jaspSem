import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName: 		"SEMSimple"
		newFunctionName:	"SEM"
		fromVersion:		"0.14.3"
		toVersion:			"0.15"

		ChangeJS
		{
			name:		"factorStandardisation"
			jsFunction:	function(options)
			{
				switch(options["factorStandardisation"])
				{
					case "factorLoadings":		return "auto.fix.first";
					case "residualVariance":	return "std.lv";
					default:					return options["factorStandardisation"]
				}
			}
		}

		ChangeSetValue
		{
			name:		"estimator"
			condition:	function(options) { return options["estimator"] === "automatic"; }
			jsonValue:	"default"
		}

		ChangeSetValue
		{
			name:		"emulation"
			condition:	function(options) { return options["emulation"] === "none"; }
			jsonValue:	"lavaan"
		}

		ChangeRename { from: "model"; to: "models" }

		ChangeJS
		{
			name:		"models"
			jsFunction:	function(options)
			{
				return [
							{
								modelName:	"Model 1",
								syntax:
								{
									modelOriginal:	options["models"]
								}
							}

						];
			}
		}
	}

	Upgrade
	{
		functionName: "LatentGrowthCurve"
		fromVersion: "0.16.4"
		toVersion: "0.17.0"

		ChangeRename {	from:	"covar";						to:		"covaryingLatentCurve"				}
		ChangeRename {	from:	"outputAdditionalFitMeasures";	to:		"additionalFitMeasures"				}
		ChangeRename {	from:	"rsquared";						to:		"rSquared"							}
		ChangeRename {	from:	"std";							to:		"standardizedEstimate"				}
		ChangeRename {	from:	"impliedCov";					to:		"impliedCovariance"					}
		ChangeRename {	from:	"residCov";						to:		"residualCovariance"				}
		ChangeRename {	from:	"showSyntax";					to:		"syntax"							}
		ChangeRename {	from:	"curveplot";					to:		"curvePlot"							}
		ChangeRename {	from:	"plot_categorical";				to:		"curvePlotCategorical"				}
		ChangeRename {	from:	"plot_max_n";					to:		"curvePlotMaxLines"					}
		ChangeRename {	from:	"misfitplot";					to:		"misfitPlot"						}
		ChangeRename {	from:	"pathplot";						to:		"pathPlot"							}
		ChangeRename {	from:	"plotpars";						to:		"pathPlotParameter"					}
		ChangeRename {	from:	"plotmeans";					to:		"pathPlotMean"						}
		ChangeRename {	from:	"mimic";						to:		"emulation"							}

		ChangeJS
		{
			name:		"emulation"
			jsFunction:	function(options)
			{
				switch(options["emulation"])
				{
					case "Mplus":	return "mplus";
					case "EQS":		return "eqs";
					default:		return options["emulation"];
				}
			}
		}

		ChangeRename {	from:	"missing";						to:		"naAction"							}
		ChangeRename {	from:	"ciWidth";						to:		"ciLevel"							}
		ChangeRename {	from:	"se";							to:		"errorCalculationMethod"			}
		ChangeRename {	from:	"bootstrapNumber";				to:		"bootstrapSamples"					}

		ChangeJS
		{
			name:		"estimator"
			jsFunction:	function(options)
			{
				switch(options["estimator"])
				{
					case "ML":		return "ml";
					case "GLS":		return "gls";
					case "WLS":		return "wls";
					case "ULS":		return "uls";
					case "DWLS":	return "dwls";
					default:		return options["estimator"];
				}
			}
		}

		ChangeRename {	from:	"fixManifestInterceptsToZero";	to:		"manifestInterceptFixedToZero"		}
		ChangeRename {	from:	"fixlatentInterceptsToZero";	to:		"latentInterceptFixedToZero"		}
		ChangeRename {	from:	"omitResidualSingleIndicator";	to:		"residualSingleIndicatorOmitted"	}
		ChangeRename {	from:	"residualVariances";			to:		"residualVariance"					}
		ChangeRename {	from:	"correlateExogenousLatents";	to:		"exogenousLatentCorrelation"		}
		ChangeRename {	from:	"addThresholds";				to:		"threshold"							}
		ChangeRename {	from:	"addScalingParameters";			to:		"scalingParameter"					}
		ChangeRename {	from:	"correlateDependentVariables";	to:		"dependentCorrelation"				}
		ChangeRename {	from:	"groupvar";						to:		"group"								}
	}

	Upgrade
	{
		functionName: "MediationAnalysis"
		fromVersion: "0.16.4"
		toVersion: "0.17.0"

		ChangeRename {	from:	"predictor";					to:		"predictors"						}
		ChangeRename {	from:	"dependent";					to:		"outcomes"							}
		ChangeRename {	from:	"std";							to:		"standardizedEstimate"				}
		ChangeRename {	from:	"showSyntax";					to:		"syntax"							}
		ChangeRename {	from:	"rsquared";						to:		"rSquared"							}
		ChangeRename {	from:	"showtotind";					to:		"totalIndirectEffect"				}
		ChangeRename {	from:	"showres";						to:		"residualCovariance"				}
		ChangeRename {	from:	"showPathCoefficients";			to:		"pathCoefficient"					}
		ChangeRename {	from:	"ciWidth";						to:		"ciLevel"							}
		ChangeRename {	from:	"se";							to:		"errorCalculationMethod"			}
		ChangeRename {	from:	"bootstrapNumber";				to:		"bootstrapSamples"					}
		ChangeRename {	from:	"bootCItype";					to:		"bootstrapCiType"					}

		ChangeJS
		{
			name:		"bootstrapCiType"
			jsFunction:	function(options)
			{
				switch(options["bootstrapCiType"])
				{
					case "bca.simple":	return "percentileBiasCorrected";
					case "perc":		return "percentile";
					case "norm":		return "normalTheory";
				}
			}
		}

		ChangeRename {	from:	"pathplot";						to:		"pathPlot"							}
		ChangeRename {	from:	"plotpars";						to:		"pathPlotParameter"					}
		ChangeRename {	from:	"plotlegend";					to:		"pathPlotLegend"					}
		ChangeRename {	from:	"missing";						to:		"naAction"							}
		ChangeRename {	from:	"mimic";						to:		"emulation"							}

		ChangeJS
		{
			name:		"emulation"
			jsFunction:	function(options)
			{
				switch(options["emulation"])
				{
					case "Mplus":	return "mplus";
					case "EQS":		return "eqs";
					default:		return options["emulation"];
				}
			}
		}

		ChangeJS
		{
			name:		"estimator"
			jsFunction:	function(options)
			{
				switch(options["estimator"])
				{
					case "ML":		return "ml";
					case "GLS":		return "gls";
					case "WLS":		return "wls";
					case "ULS":		return "uls";
					case "DWLS":	return "dwls";
					default:		return options["estimator"];
				}
			}
		}
	}

	Upgrade
	{
		functionName: "MIMIC"
		fromVersion: "0.16.4"
		toVersion: "0.17.0"

		ChangeRename {	from:	"std";							to:		"standardizedEstimate"				}
		ChangeRename {	from:	"showSyntax";					to:		"syntax"							}
		ChangeRename {	from:	"rsquared";						to:		"rSquared"							}
		ChangeRename {	from:	"additionalfits";				to:		"additionalFitMeasures"				}
		ChangeRename {	from:	"ciWidth";						to:		"ciLevel"							}
		ChangeRename {	from:	"se";							to:		"errorCalculationMethod"			}
		ChangeRename {	from:	"bootstrapNumber";				to:		"bootstrapSamples"					}
		ChangeRename {	from:	"bootCItype";					to:		"bootstrapCiType"					}

		ChangeJS
		{
			name:		"bootstrapCiType"
			jsFunction:	function(options)
			{
				switch(options["bootstrapCiType"])
				{
					case "bca.simple":	return "percentileBiasCorrected";
					case "perc":		return "percentile";
					case "norm":		return "normalTheory";
				}
			}
		}

		ChangeRename {	from:	"pathplot";						to:		"pathPlot"							}
		ChangeRename {	from:	"plotpars";						to:		"pathPlotParameter"					}
		ChangeRename {	from:	"plotlegend";					to:		"pathPlotLegend"					}
		ChangeRename {	from:	"missing";						to:		"naAction"							}
		ChangeRename {	from:	"mimic";						to:		"emulation"							}

		ChangeJS
		{
			name:		"emulation"
			jsFunction:	function(options)
			{
				switch(options["emulation"])
				{
					case "Mplus":	return "mplus";
					case "EQS":		return "eqs";
					default:		return options["emulation"];
				}
			}
		}

		ChangeJS
		{
			name:		"estimator"
			jsFunction:	function(options)
			{
				switch(options["estimator"])
				{
					case "ML":		return "ml";
					case "GLS":		return "gls";
					case "WLS":		return "wls";
					case "ULS":		return "uls";
					case "DWLS":	return "dwls";
					default:		return options["estimator"];
				}
			}
		}
	}

	Upgrade
	{
		functionName: "SEM"
		fromVersion: "0.16.4"
		toVersion: "0.17.0"
		ChangeJS
		{
			name: "models"
			jsFunction: function(options)
			{
				let newModels = options["models"].map(model => {
					let newModel = {};
					newModel.name			= model.modelName;
					newModel.syntax			= model.syntax;
					return newModel;
				});
				return newModels;
			}
		}

		ChangeRename {	from:	"Data";							to:		"dataType"							}
		ChangeJS
		{
			name:		"dataType"
			jsFunction:	function(options) 
			{
				switch(options["dataType"])
				{
					case "varcov":	return "varianceCovariance";
					default:		return options["dataType"];
				}
			}
		}
		ChangeRename {	from:	"SampleSize";					to:		"sampleSize"						}
		ChangeRename {	from:	"sampling.weights";				to:		"samplingWeights"					}
		ChangeRename {	from:	"outputAdditionalFitMeasures";	to:		"additionalFitMeasures"				}
		ChangeRename {	from:	"outputRSquared";				to:		"rSquared"							}
		ChangeRename {	from:	"outputObservedCovariances";	to:		"observedCovariance"				}
		ChangeRename {	from:	"outputImpliedCovariances";		to:		"impliedCovariance"					}
		ChangeRename {	from:	"outputResidualCovariances";	to:		"residualCovariance"				}
		ChangeRename {	from:	"outputStandardizedResiduals";	to:		"standardizedResidual"				}
		ChangeRename {	from:	"outputMardiasCoefficients";	to:		"mardiasCoefficient"				}
		ChangeRename {	from:	"std";							to:		"standardizedEstimate"				}
		ChangeRename {	from:	"outputPathPlot";				to:		"pathPlot"							}
		ChangeRename {	from:	"pathPlotPar";					to:		"pathPlotParameter"					}
		ChangeRename {	from:	"outputModificationIndices";	to:		"modificationIndex"					}
		ChangeRename {	from:	"miHideLow";					to:		"modificationIndexHiddenLow"		}
		ChangeRename {	from:	"miThreshold";					to:		"modificationIndexThreshold"		}
		ChangeRename {	from:	"factorStandardisation";		to:		"factorScaling"						}

		ChangeJS
		{
			name:		"factorScaling"
			jsFunction:	function(options)
			{
				switch(options["factorScaling"])
				{
					case "auto.fix.first":	return "factorLoading";
					case "std.lv":			return "factorVariance";
					case "effect.coding":	return "effectCoding";
					default:				return options["factorScaling"]
				}
			}
		}

		ChangeRename {	from:	"meanstructure";						to:		"meanStructure"						}
		ChangeRename {	from:	"int.ov.fixed";							to:		"manifestInterceptFixedToZero"		}
		ChangeRename {	from:	"int.lv.fixed";							to:		"latentInterceptFixedToZero"		}
		ChangeRename {	from:	"fixed.x";								to:		"exogenousCovariateFixed"			}
		ChangeRename {	from:	"auto.fix.single";						to:		"residualSingleIndicatorOmitted"	}
		ChangeRename {	from:	"auto.var";								to:		"residualVariance"					}
		ChangeRename {	from:	"auto.cov.lv.x";						to:		"exogenousLatentCorrelation"		}
		ChangeRename {	from:	"auto.cov.y";							to:		"dependentCorrelation"				}
		ChangeRename {	from:	"auto.th";								to:		"threshold"							}
		ChangeRename {	from:	"auto.delta";							to:		"scalingParameter"					}
		ChangeRename {	from:	"auto.efa";								to:		"efaConstrained"					}
		ChangeRename {	from:	"information";							to:		"informationMatrix"					}
		ChangeRename {	from:	"se";									to:		"errorCalculationMethod"			}
		ChangeRename {	from:	"errorCalculationBootstrapSamples";		to:		"bootstrapSamples"					}
		ChangeRename {	from:	"bootCItype";							to:		"bootstrapCiType"					}

		ChangeJS
		{
			name:		"bootstrapCiType"
			jsFunction:	function(options)
			{
				switch(options["bootstrapCiType"])
				{
					case "bca.simple":	return "percentileBiasCorrected";
					case "perc":		return "percentile";
					case "norm":		return "normalTheory";
				}
			}
		}

		ChangeRename {	from:	"ciWidth";						to:		"ciLevel"							}
		ChangeRename {	from:	"std.ov";						to:		"standardizedVariable"				}

		ChangeJS
		{
			name:		"estimator"
			jsFunction:	function(options)
			{
				switch(options["estimator"])
				{
					case "ML":		return "ml";
					case "GLS":		return "gls";
					case "WLS":		return "wls";
					case "ULS":		return "uls";
					case "DWLS":	return "dwls";
					case "PML":		return "pml";
					default:		return options["estimator"];
				}
			}
		}

		ChangeRename {	from:	"test";							to:		"modelTest"							}

		ChangeJS
		{
			name:		"modelTest"
			jsFunction:	function(options)
			{
				switch(options["modelTest"])
				{
					case "Satorra.Bentler":		return "satorraBentler";
					case "Yuan.Bentler":		return "yuanBentler";
					case "mean.var.adjusted":	return "meanAndVarianceAdjusted";
					case "scaled.shifted":		return "scaledAndShifted";
					case "Bollen.Stine":		return "bollenStine";
					default:					return options["modelTest"];
				}
			}
		}

		ChangeRename {	from:	"missing";						to:		"naAction"							}

		ChangeJS
		{
			name:		"naAction"
			jsFunction:	function(options)
			{
				switch(options["naAction"])
				{
					case "ml":					return "fiml";
					case "two.stage":			return "twoStage";
					case "robust.two.stage":	return "twoStageRobust";
					case "doubly.robust":		return "doublyRobust";
					default:					return options["naAction"];
				}
			}
		}

		ChangeJS
		{
			name:		"emulation"
			jsFunction:	function(options)
			{
				switch(options["emulation"])
				{
					case "Mplus":	return "mplus";
					case "EQS":		return "eqs";
					default:		return options["emulation"];
				}
			}
		}

		ChangeRename {	from:	"groupingVariable";				to:		"group"								}
		ChangeRename {	from:	"eq_loadings";					to:		"equalLoading"						}
		ChangeRename {	from:	"eq_intercepts";				to:		"equalIntercept"					}
		ChangeRename {	from:	"eq_residuals";					to:		"equalResidual"						}
		ChangeRename {	from:	"eq_residualcovariances";		to:		"equalResidualCovariance"			}
		ChangeRename {	from:	"eq_means";						to:		"equalMean"							}
		ChangeRename {	from:	"eq_thresholds";				to:		"equalThreshold"					}
		ChangeRename {	from:	"eq_regressions";				to:		"equalRegression"					}
		ChangeRename {	from:	"eq_variances";					to:		"equalLatentVariance"				}
		ChangeRename {	from:	"eq_lvcovariances";				to:		"equalLatentCovariance"				}
		ChangeRename {	from:	"group.partial";				to:		"freeParameters"					}
	}


}
