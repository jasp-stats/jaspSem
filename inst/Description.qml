import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"jaspSem"
	title : 		qsTr("SEM")
	description:	qsTr("Evaluate latent data structures with Yves Rosseel’s lavaan program")
	icon:			"sem-latreg.svg"
	version			: "0.19.0"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"https://github.com/jasp-stats/jaspSem/"
	license:		"GPL (>= 2)"
	hasWrappers:	true

	Analysis
	{
		title:	qsTr("Structural Equation Modeling")
		qml:	"SEM.qml"
		func:	"SEMInternal"
	}

	Analysis
	{
		title:	qsTr("Partial Least Squares SEM")
		qml:	"PLSSEM.qml"
		func:	"PLSSEMInternal"
	}

	Analysis
	{
		title:	qsTr("Mediation Analysis")
		qml:	"MediationAnalysis.qml"
		func:	"MediationAnalysisInternal"
	}

	Analysis
	{
		title:	qsTr("MIMIC Model")
		qml:	"Mimic.qml"
		func:	"MIMICInternal"
	}

	Analysis
	{
		title:	qsTr("Latent Growth")
		qml:	"LatentGrowthCurve.qml"
		func:	"LatentGrowthCurveInternal"
		//enabled: false
	}
}
