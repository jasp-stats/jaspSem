import QtQuick		2.12
import JASP.Module	1.0

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
					case "automatic":		return "default";
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
					case "none":		return "lavaan";
				}
			}
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
}
