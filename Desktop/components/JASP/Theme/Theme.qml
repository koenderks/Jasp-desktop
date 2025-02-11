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
import QtQuick.Controls
import JASP

JaspTheme
{
	themeName:	"lightTheme" // *The original* //

	//Fonts:
	font.pixelSize:										Math.round(12 * uiScale)
	font.family:										preferencesModel.interfaceFont		//If you change the property .family the font won't respond to the UI anymore unless you use jaspFont in it somewhere...
	font.weight:										Font.Normal

	fontLink.pixelSize:									Math.round(12 * uiScale)
	fontLink.family:									preferencesModel.interfaceFont
	fontLink.weight:									Font.Normal
	fontLink.underline:									true

	fontLabel.bold:										true
	fontLabel.pixelSize:								Math.round(18 * uiScale)
	fontLabel.family:									preferencesModel.interfaceFont
	fontLabel.weight:									Font.Bold

	fontRibbon.bold:									false
	fontRibbon.pixelSize:								Math.round(14 * uiScale)
	fontRibbon.family:									preferencesModel.interfaceFont
	fontRibbon.weight:									Font.DemiBold

	fontGroupTitle.bold:								true
	fontGroupTitle.pixelSize:							Math.round(14 * uiScale)
	fontGroupTitle.family:								preferencesModel.interfaceFont
	fontGroupTitle.weight:								Font.Medium

	fontALTNavTag.bold:									true
	fontALTNavTag.pixelSize:							Math.round(15 * uiScale)
	fontALTNavTag.family:								preferencesModel.interfaceFont
	fontALTNavTag.weight:								Font.Medium

	fontPrefOptionsGroupTitle.bold:						true
	fontPrefOptionsGroupTitle.pixelSize:				Math.round(13 * uiScale)
	fontPrefOptionsGroupTitle.family:					preferencesModel.interfaceFont
	fontPrefOptionsGroupTitle.weight:					Font.Bold

	fontRCode.family:									preferencesModel.codeFont
	fontRCode.pixelSize:								Math.round(14 * uiScale)
	fontRCode.weight:									Font.Normal

	fontCode.family:									preferencesModel.codeFont
	fontCode.pixelSize:									Math.round(11 * uiScale)
	fontCode.weight:									Font.Normal

	//Scales:
	ribbonScaleHovered:					1.1

	//Color definitions:
	white:								"white"
	whiteBroken:						"#F5F5F5"
	black:								"black"

	grayDarker:							"#9A9A9A"
	gray:								"#d1d1d1"
	grayLighter:						"#E0E0E0"
	grayMuchLighter:					"#ECECEC"
	grayVeryMuchLighter:				"#F4F6F7"

	blueDarker:							"#0069D9"
	blue:								jaspBlue//"#419BF9"
	blueLighter:						"#97C4F2"
	blueMuchLighter:					"#DCF1FB"

	red:								"#FC625D"
	redDarker:							"#CD0A0A"

	green:								"#36CE4C"

	yellowLight:						"#FFFFCA"

	rose:								"#FFC0CB"
	roseLight:							"#FEF1EC"

	cyan:								"#39CEF9"

	shadow:								"#33000000"

	jaspBlue:							"#14a1e3"
	jaspGreen:							"#8cc63e"

	textEnabled:						black
	textDisabled:						grayDarker

	uiBackground:						grayMuchLighter
	uiBorder:							grayDarker

	fileMenuColorBackground:			grayVeryMuchLighter
	fileMenuLightBorder:				grayLighter

	buttonColor:						grayVeryMuchLighter
	buttonColorHovered:					grayLighter
	buttonColorPressed:					gray
	buttonColorDisabled:				grayLighter
	buttonBorderColor:					gray
	buttonBorderColorHovered:			grayDarker

	itemHighlight:						focusBorderColor
	itemHoverColor:						blueMuchLighter
	itemSelectedColor:					blueDarker
	itemSelectedNoFocusColor:			grayLighter

	altNavTagColor:						black

	//JASPControl colors mostly:
	borderColor:						gray
	focusBorderColor:					blueLighter
	dependencyBorderColor:				green
	dependencySelectedColor:			cyan
	containsDragBorderColor:			green

	analysisBackgroundColor:			grayMuchLighter
	controlBackgroundColor:				white
	controlDisabledBackgroundColor:		whiteBroken
	rowEvenColor:						controlBackgroundColor
	rowOnevenColor:						whiteBroken
	controlErrorBackgroundColor:		roseLight
	controlErrorTextColor:				redDarker
	controlWarningBackgroundColor:		"#FD0"
	controlWarningTextColor:			"#B70"

	buttonBackgroundColor:				blue
	tooltipBackgroundColor:				yellowLight
	debugBackgroundColor:				rose
	errorMessagesBackgroundColor:		red
	sliderPartOn:						blue
	sliderPartOff:						grayDarker

	darkeningColour:					"black"

	//Distances:
	borderRadius:							4
	shadowRadius:							10

	itemPadding:							8
	jaspControlPadding:						3
	ribbonButtonPadding:					10
	groupContentPadding:					10

	rowSpacing:								12
	rowGridSpacing:							15
	rowGroupSpacing:						5
	columnGridSpacing:						30
	columnGroupSpacing:						10
	indentationLength:						20
	labelSpacing:							4
	menuSpacing:							1
	menuPadding:							10

	generalAnchorMargin:					8
	generalMenuMargin:						12
	titleBottomMargin:						5
	contentMargin:							4
	subOptionOffset:						40

	//Sizes:
	minPanelWidth:							200
	resultWidth:							600
	formWidth:								625
	iconSize:								16
	formMargin:								10
	formExpanderHeaderHeight:				40
	sliderWidth:							4
	sliderLength:							80
	switchHeight:							15
	spinBoxWidth:							50
	spinBoxHeight:							30
	comboBoxHeight:							20
	textFieldWidth:							200
	textFieldHeight:						20
	numericFieldWidth:						40
	splitHandleWidth:						22
	subMenuIconHeight:						13
	ribbonButtonHeight:						72
	variablesListTitle:						16
	sliderHandleDiameter:					16
	defaultTextAreaHeight:					250
	jaspControlHighlightWidth:				3
	defaultVariablesFormHeight:				350
	defaultSingleItemListHeight:			44
	defaultRectangularButtonHeight:			32
	smallDefaultVariablesFormHeight:		200
	messageBoxButtonHeight:					40
	scrollbarBoxWidthBig:					16
	scrollbarBoxWidth:						12
	menuItemHeight:							20
	menuGroupTitleHeight:					40
	menuHeaderHeight:						50
	
}
