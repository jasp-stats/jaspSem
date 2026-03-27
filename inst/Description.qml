import QtQuick
import JASP.Module

Description
{
	title : 		qsTr("SEM")
	description:	qsTr("Evaluate latent data structures with Yves Rosseel’s lavaan program")
	icon:			"sem-latreg.svg"
	hasWrappers: 	true

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"sem-latreg.svg"
	}

	Analysis
	{
		title:	qsTr("Structural Equation Modeling")
		qml:	"SEM.qml"
		func:	"SEM"
		preloadData: false
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
		preloadData: false
	}

	Analysis
	{
		title:	qsTr("MIMIC Model")
		qml:	"Mimic.qml"
		func:	"MIMIC"
		preloadData: false
	}

	Analysis
	{
		title:	qsTr("Latent Growth")
		qml:	"LatentGrowthCurve.qml"
		func:	"LatentGrowthCurve"
		preloadData: false
	}

	Analysis
	{
		title: qsTr("MNLFA")
		qml: "ModeratedNonLinearFactorAnalysis.qml"
		func: "ModeratedNonLinearFactorAnalysis"
		preloadData: true
	}
	
	Separator {}
	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"sem-latreg-bayesian.svg"
	}
	Analysis
	{
		title:	qsTr("Bayesian Structural Equation Modeling")
		menu: qsTr("Structural Equation Modeling")
		qml:	"BayesianSEM.qml"
		func:	"BayesianSEM"		
		preloadData: true
	}
}
