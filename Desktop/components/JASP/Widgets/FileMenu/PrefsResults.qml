import QtQuick
import QtQuick.Controls
import JASP.Widgets
import JASP.Controls

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) displayExactPVals.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Results Preferences")
			helpfile:		"preferences/PrefsResults"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}


		PrefsGroupRect
		{
			title:			qsTr("Table options")

			CheckBox
			{
				id:						displayExactPVals
				label:					qsTr("Display exact p-values")
				checked:				preferencesModel.exactPValues
				onCheckedChanged:		preferencesModel.exactPValues = checked
				
				KeyNavigation.tab:			useNormalizedNotation
			}

			CheckBox
			{
				id:						useNormalizedNotation
				label:					qsTr("Use exponent notation") //the default now is normalizedNotation. https://github.com/jasp-stats/INTERNAL-jasp/issues/1872
				checked:				!preferencesModel.normalizedNotation
				onCheckedChanged:		preferencesModel.normalizedNotation = !checked
				KeyNavigation.tab:		fixDecs

			}

			Item
			{
				height:						fixDecs.height
				width:						fixDecs.width + numDecs.width

				CheckBox
				{
					id:						fixDecs
					label:					qsTr("Fix the number of decimals")
					checked:				preferencesModel.fixedDecimals
					onCheckedChanged:		preferencesModel.fixedDecimals = checked
					
					KeyNavigation.tab:			numDecs
				}

				SpinBox
				{
					id:						numDecs
					value:					preferencesModel.numDecimals
					onValueChanged:			preferencesModel.numDecimals = value
					enabled:				preferencesModel.fixedDecimals

					KeyNavigation.tab:			useDefaultPPICheckbox

					anchors
					{
						left:				fixDecs.right
						leftMargin:			jaspTheme.generalAnchorMargin
						verticalCenter:		parent.verticalCenter
					}
				}
			}
		}
		
		PrefsGroupRect
		{
			title:				qsTr("Plot options")

			CheckBox
			{
				id:					useDefaultPPICheckbox
				label:				qsTr("Use PPI of screen in plots: ") + "(" + preferencesModel.defaultPPI + ")"
				checked:			preferencesModel.useDefaultPPI
				onCheckedChanged:	preferencesModel.useDefaultPPI = checked
				height:				implicitHeight * preferencesModel.uiScale
				toolTip:			qsTr("Use the Pixels Per Inch of your screen to render your plots.")
				focus:				true

				KeyNavigation.tab:		customPPISpinBox
			}

			SpinBox
			{
				id:						customPPISpinBox
				value:					preferencesModel.customPPI
				onValueChanged:			preferencesModel.customPPI = value
				from:					16
				to:						2000
				stepSize:				16

				text:					qsTr("Custom PPI: ")
				enabled:				!preferencesModel.useDefaultPPI

				x:						jaspTheme.subOptionOffset

				KeyNavigation.tab:			whiteBackgroundButton
			}

			RadioButtonGroup
			{
				title:					qsTr("Image background color")

				RadioButton
				{
					id:					whiteBackgroundButton
					text:				qsTr("White")
					checked:			preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = checked
					toolTip:			qsTr("This makes the background of all plots white, quite useful if you want to use it in LaTeX or submit it to a journal.")

					KeyNavigation.tab:      transparentBackgroundButton
				}

				RadioButton
				{
					id:					transparentBackgroundButton
					text:				qsTr("Transparent")
					checked:			!preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = !checked
					toolTip:			qsTr("This makes the background of all plots transparent, quite useful if you want to use it seamlessly on any background that isn't white.")

					KeyNavigation.tab:		pdfOrientationLandscape
				}
			}
		}

		PrefsGroupRect
		{
			title:				qsTr("PDF Settings")

			RadioButtonGroup
			{
				id:		pdfOrientation
				title:	qsTr("Orientation")

				RadioButton
				{
					id:					pdfOrientationPortrait
					label:				qsTr("Portrait")
					checked:			!preferencesModel.pdfLandscape
					onCheckedChanged:	if (checked) preferencesModel.pdfLandscape = false

					KeyNavigation.tab:	pdfOrientationLandscape
				}

				RadioButton
				{
					id:					pdfOrientationLandscape
					label:				qsTr("Landscape")
					checked:			preferencesModel.pdfLandscape
					onCheckedChanged:	if (checked) preferencesModel.pdfLandscape = true

					KeyNavigation.tab:	pdfPageSize
				}
			}

			DropDown
			{
				id:				pdfPageSize
				label:			qsTr("Page size")
				values:			preferencesModel.pdfPageSizeModel
				startValue: 	preferencesModel.pdfPageSize
				onValueChanged: preferencesModel.pdfPageSize = value

				KeyNavigation.tab:		showRSyntaxInResults
			}

		}

		PrefsGroupRect
		{
			title:				qsTr("Miscellaneous options")

			CheckBox
			{
				id:					showRSyntaxInResults
				label:				qsTr("Show R syntax")
				checked:			preferencesModel.showRSyntaxInResults
				onCheckedChanged:	preferencesModel.showRSyntaxInResults = checked
				height:				implicitHeight * preferencesModel.uiScale
				toolTip:			qsTr("Add R syntax for each analysis")
				focus:				true

				KeyNavigation.tab:		displayExactPVals
			}
		}

		Item
		{
			id:		extraSpaceForScrolling
			width:	1
			height:	1
		}
	}
}
