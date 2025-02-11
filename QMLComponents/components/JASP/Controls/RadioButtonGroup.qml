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
import QtQuick.Layouts	as L
import JASP.Controls


RadioButtonsGroupBase
{
	id:					control
	childControlsArea:	contentArea
	focusOnTab:			false
	shouldStealHover:	false

	default property alias	content:				contentArea.children
			property bool	radioButtonsOnSameRow:	false
			property alias	columns:				contentArea.columns
			property alias	text:					control.title
			property int	leftPadding:			jaspTheme.groupContentPadding
			property int	textFormat:				Text.AutoText

	implicitWidth:	radioButtonsOnSameRow
						? contentArea.x + contentArea.implicitWidth
						: Math.max(label.implicitWidth, contentArea.x + contentArea.implicitWidth)

	implicitHeight:	radioButtonsOnSameRow
						? Math.max(label.implicitHeight, contentArea.implicitHeight)
						: contentArea.y + contentArea.implicitHeight	
    
	L.Layout.leftMargin:	indent ? jaspTheme.indentationLength : 0
	
	Label
	{
		id:				label
		text:			control.title
		visible:		control.title && control.visible ? true : false
		anchors.top:	control.top
		anchors.left:	control.left
		anchors.bottom:	radioButtonsOnSameRow ? contentArea.bottom : undefined
		verticalAlignment: Text.AlignVCenter
		font:			jaspTheme.font
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled		
		textFormat:		control.textFormat
    }
    

	GridLayout
	{
		id:					contentArea
		rowSpacing:			jaspTheme.rowGroupSpacing
		columnSpacing:		jaspTheme.columnGroupSpacing
		columns:			radioButtonsOnSameRow ? children.length : 1
		anchors.top:		control.title && !radioButtonsOnSameRow ? label.bottom : control.top
		anchors.topMargin:	control.title && !radioButtonsOnSameRow ? jaspTheme.titleBottomMargin : 0
		anchors.left:		control.title && radioButtonsOnSameRow ? label.right : control.left
		anchors.leftMargin: control.title ? jaspTheme.groupContentPadding : 0
	}

	background: backgroundBox

	Rectangle
	{
		// This rectangle is only here to show the dependency outline for "Show Dependencies"
		id:					backgroundBox
		color:				"transparent"
		border.width:		0
		anchors.fill:		parent
		anchors.margins:	-1 * border.width
		z:					-1
		visible:			preferencesModel.developerMode
	}
}
