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


Rectangle
{
	id							: ribbonButton
	width						: implicitWidth
	height						: implicitHeight
	implicitHeight				: jaspTheme.ribbonButtonHeight * 0.6
	implicitWidth				: implicitHeight
	// radius					: 5
	color						: pressed ? jaspTheme.grayLighter : "transparent"
	
	enum ButtonType
	{
		Hamburger,
		Plus,
		Tools,
		LeftArrow,
		RightArrow
	}
	
	property int buttonType:	MenuArrowButton.ButtonType.Hamburger 
	property string	toolTip:	""
	
			 property double iconScale:	0.7
			 property bool showPressed: false
	readonly property bool pressed:		mice.pressed || showPressed
	readonly property bool hamburger:	buttonType == MenuArrowButton.ButtonType.Hamburger || buttonType == MenuArrowButton.ButtonType.LeftArrow
	readonly property bool showArrow:	buttonType == MenuArrowButton.ButtonType.LeftArrow || buttonType == MenuArrowButton.ButtonType.RightArrow 
	readonly property bool isTools:		buttonType == MenuArrowButton.ButtonType.Tools


	ToolTip.text:				toolTip
	ToolTip.timeout:			jaspTheme.toolTipTimeout
	ToolTip.delay:				jaspTheme.toolTipDelay
	ToolTip.visible:			toolTip !== "" && mice.containsMouse

	signal clicked

	Item
	{
		id:					hamburgerArrow
		anchors.centerIn:	parent
		width:				hamburgerArrow.barWidth//parent.width	- (2 * jaspTheme.ribbonButtonPadding)
		height:				baseHeight - 20
		scale:				baseScale * (mice.containsMouse && !ribbonButton.pressed ? jaspTheme.ribbonScaleHovered : 1)


		property real	baseScale:		iconScale * (parent.height / baseHeight)//Ok changing height doesnt work well for this component so I just scale it when necessary!

		property real	baseHeight:		80
		property real	barThickness:	8 //(jaspTheme.ribbonButtonHeight (2 * jaspTheme.ribbonButtonPadding)) / 7
		property real	barRadius:		barThickness
		property real	barWidth:		baseHeight / 2
		property color	barColor:		jaspTheme.jaspBlue

		Image
		{
			id:				toolsIcon
			source:			jaspTheme.iconPath + "/workspace-tools.svg"
			width:			parent.width
			height:			width
			anchors.centerIn: parent

			visible:		isTools
		}

		Item
		{
			id:					topBar
			anchors.centerIn:	parent
			height:				hamburgerArrow.barThickness
			width:				parent.width
			visible:			!isTools

			Rectangle
			{
				anchors.centerIn:	ribbonButton.showArrow  ||  ribbonButton.hamburger ? undefined		: parent
				anchors.left:		!ribbonButton.showArrow || !ribbonButton.hamburger ? undefined	: parent.left
				anchors.right:		!ribbonButton.showArrow ||  ribbonButton.hamburger ? undefined	: parent.right
				transformOrigin:	!ribbonButton.showArrow ? Item.Center	: ribbonButton.hamburger ? Item.Left	: Item.Right

				y:					ribbonButton.showArrow ? height * 0.25 : ribbonButton.hamburger ? hamburgerArrow.barThickness * 2 : 0

				height:				parent.height
				width:				!ribbonButton.showArrow ? parent.width : parent.width / 1.25
				rotation:			!ribbonButton.showArrow ?
										(ribbonButton.hamburger ?   0 : 90) :
										(ribbonButton.hamburger ? -45 : 45)
				radius:				hamburgerArrow.barRadius
				color:				hamburgerArrow.barColor
			}
		}

		Rectangle
		{
			id:		middleBar
			height:	hamburgerArrow.barThickness
			width:	parent.width
			radius:	hamburgerArrow.barRadius
			color:	hamburgerArrow.barColor
			visible:			!isTools

			anchors.centerIn:	parent
		}

		Item
		{
			id:					bottomBar
			anchors.centerIn:	parent
			height:				hamburgerArrow.barThickness
			width:				parent.width
			visible:			!isTools

			Rectangle
			{
				anchors.centerIn:	 ribbonButton.showArrow ||  ribbonButton.hamburger	? undefined		: parent
				anchors.left:		!ribbonButton.showArrow || !ribbonButton.hamburger	? undefined		: parent.left
				anchors.right:		!ribbonButton.showArrow ||  ribbonButton.hamburger	? undefined		: parent.right
				transformOrigin:	!ribbonButton.showArrow								? Item.Center	: ribbonButton.hamburger ? Item.Left	: Item.Right

				y:					ribbonButton.showArrow ? -height * 0.25 : ribbonButton.hamburger ? -hamburgerArrow.barThickness * 2 : 0

				height:				parent.height
				width:				!ribbonButton.showArrow ? parent.width : parent.width / 1.25
				rotation:			!ribbonButton.showArrow ? 0 : ribbonButton.hamburger ? 45 : -45
				radius:				hamburgerArrow.barRadius
				color:				hamburgerArrow.barColor
			}

		}
	}

	MouseArea
	{
		id						: mice
		anchors.fill			: parent
		hoverEnabled			: true
		acceptedButtons			: Qt.LeftButton
		onClicked				: ribbonButton.clicked();
		cursorShape				: Qt.PointingHandCursor
	}
	
	Rectangle
    {
        id      : borderLeft
        width   : 1
		color   : jaspTheme.uiBorder
		visible	: pressed
        anchors
        {
            left	: parent.left
            top		: parent.top
            bottom	: parent.bottom
        }
		
    }

    Rectangle
    {
        id      : borderRight
        width   : 1
		color   : jaspTheme.uiBorder
		visible	: pressed
        anchors
        {
            right	: parent.right
            top		: parent.top
            bottom	: parent.bottom
        }
    }
}
