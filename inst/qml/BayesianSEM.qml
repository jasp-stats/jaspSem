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

		content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan; showLineNumber: true }
	}

	RadioButtonGroup
	{
		title: qsTr("Data")
		name: "dataType"
		columns: 2
		RadioButton { value: "raw"; label: qsTr("Raw"); checked: true }
	}

	DropDown
	{
		name: "group"
		label: qsTr("Grouping variable")
		showVariableTypeIcon: true
		addEmptyValue: true
	}

	Section
	{
		title: qsTr("Model Options")
		Group
		{
			DropDown
			{
				name: "factorScaling"
				label: qsTr("Factor scaling")
				values:
				[
					{ label: qsTr("Factor loadings"),	value: "factorLoading"	},
					{ label: qsTr("Factor variance"),	value: "factorVariance"	},
					{ label: qsTr("None"),				value: "none"			}
				]
			}

			CheckBox { name: "manifestInterceptFixedToZero"; 	label: qsTr("Fix manifest intercepts to zero"); checked: false }
			CheckBox { name: "latentInterceptFixedToZero"; 		label: qsTr("Fix latent intercepts to zero"); checked: true }
			CheckBox { name: "orthogonal"; 			label: qsTr("Assume factors uncorrelated"); checked: false }
		}

		Group
		{
			title: qsTr("Include mean structure")
			CheckBox { name: "meanStructure"; id: meanstructure; label: qsTr("Include mean structure"); checked: false }
		}
	}

	Section
	{
		title: qsTr("MCMC Options")
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
			}

			IntegerField
			{
				name: "mcmcSamples"
				label: qsTr("Samples")
				defaultValue: 1000
				min: 100
				max: 100000
			}

			IntegerField
			{
				name: "mcmcChains"
				label: qsTr("Chains")
				defaultValue: 3
				min: 1
				max: 10
			}

			IntegerField
			{
				name: "mcmcThin"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: 100
			}
		}

		Group
		{
			title: qsTr("Other")
			
			CIField {
				text: qsTr("Credible interval")
				name: "ciLevel"
			}

			CheckBox
			{
				name:		"userGaveSeed"
				id:			user_seed
				label:		qsTr("Set random seed:")
				checked:	false
				childrenOnSameRow: true

				IntegerField
				{
					name:			"bootSeed"
					defaultValue:	1
					min:			1
				}
			}
		}
	}

	Section
	{
		title: qsTr("Output Options")

		Group
		{
			title: qsTr("Additional output")
			CheckBox { name: "warnings"; label: qsTr("Show warnings"); checked: false }
		}
	}
}
