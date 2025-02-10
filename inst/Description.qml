import QtQuick
import JASP.Module

Description
{
	name:			"jaspSem"
	title : 		qsTr("SEM")
	description:	qsTr("Evaluate latent data structures with Yves Rosseel’s lavaan program")
	icon:			"sem-latreg.svg"
	version			: "0.19.3"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"https://github.com/jasp-stats/jaspSem/"
	license:		"GPL (>= 2)"
	hasWrappers:	true
	preloadData:	false

	Analysis
	{
		title:	qsTr("Structural Equation Modeling")
		qml:	"SEM.qml"
		func:	"SEM"
	}

	Analysis
	{
		title:	qsTr("Partial Least Squares SEM")
		qml:	"PLSSEM.qml"
		func:	"PLSSEM"
	}

	Analysis
	{
		title:	qsTr("Mediation Analysis")
		qml:	"MediationAnalysis.qml"
		func:	"MediationAnalysis"
	}

	Analysis
	{
		title:	qsTr("MIMIC Model")
		qml:	"Mimic.qml"
		func:	"MIMIC"
	}

	Analysis
	{
		title:	qsTr("Latent Growth")
		qml:	"LatentGrowthCurve.qml"
		func:	"LatentGrowthCurve"
		//enabled: false
	}
}
