import QtQuick
import JASP.Module

Description
{
	title : 		qsTr("SEM")
	description:	qsTr("Evaluate latent data structures with Yves Rosseel’s lavaan program")
	icon:			"sem-latreg.svg"
	preloadData:	false
	hasWrappers: 	true

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
		preloadData: true
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
