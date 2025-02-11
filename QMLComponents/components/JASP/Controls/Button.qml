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
import JASP.Controls

JASPControl
{
	id:					button
	controlType:		JASPControl.Button
	implicitHeight:		control.implicitHeight
	implicitWidth:		control.implicitWidth
	isBound:			false
	shouldStealHover:	false
	innerControl:		control
	focusIndicator:		focusIndicator
	title:				text

	
	readonly	property alias control:		control
				property alias text:		control.text
				property alias label:		control.text
				property alias textFormat:	control.textFormat
				property alias iconSource:	control.iconSource
	readonly	property alias pressed:		control._pressed

    signal clicked()
    
	Component.onCompleted: control.clicked.connect(clicked);

	RoundedButton
	{
		id:				control
		anchors.fill:	parent
    }

	Rectangle
	{
		id:					focusIndicator
		anchors.fill:		parent
		color:				"transparent"
		border.width:		0
		border.color:		"transparent"
		radius:				jaspTheme.jaspControlHighlightWidth
	}
}
