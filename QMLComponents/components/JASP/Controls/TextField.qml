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
import QtQuick.Controls as QTC
import QtQuick.Layouts
import JASP.Controls

TextInputBase
{
	id:					textField
	implicitHeight:		control.height
	implicitWidth:		afterLabel.text ? (afterLabelRect.x + afterLabelRect.width) : (control.x + control.width)
	background:			useExternalBorder ? externalControlBackground : control.background
	cursorShape:		Qt.IBeamCursor
	innerControl:		control
	title:				text
	
	property alias	control:			control
	property alias	text:				textField.label
	property alias	displayValue:		control.text	///< In onEditingFinished this contains the "value" entered by the user
	property int	textFormat:			Text.AutoText
	property var	lastValidValue:		defaultValue
	property int	fieldWidth:			jaspTheme.textFieldWidth
	property int	fieldHeight:		0
	property bool	useExternalBorder:	!parentListView
	property bool	showBorder:			true
	property alias	placeholderText:	control.placeholderText
	property bool	selectValueOnFocus:	false
	property alias	startValue:			textField.defaultValue
	property bool	moveFocusOnEdit:	true
	property alias	validator:			control.validator
	property alias	controlLabel:		beforeLabel
	property string	inputType:			"string"
	property bool	useLastValidValue:	true
	property bool	editable:			true
	property var	undoModel

	property double controlXOffset:		0
	property bool	alignInGroup:		true


	signal editingFinished()	///< To get the entered value use `displayValue` in the slot instead of `value`
	signal textEdited()
	signal pressed(var event)
	signal released(var event)

	function doEditingFinished()
	{
		if (displayValue === "" && defaultValue !== undefined && String(defaultValue) !== "")
			displayValue = defaultValue;
		lastValidValue = displayValue
		editingFinished();
	}

	function undo() {
		if (undoModel) {
			undoModel.undo()
			return true
		}
		else
			return false
	}
	function redo()
	{
		if (undoModel) {
			undoModel.redo()
			return true
		}
		else
			return false
	}
	
	Component.onCompleted:
	{
		if (!beforeLabel.text && textField.text)
			beforeLabel.text = textField.text;
		
		control.editingFinished.connect(doEditingFinished);
		control.textEdited.connect(textEdited);
		control.pressed.connect(pressed);
		control.released.connect(released);
		if (control.text)
			lastValidValue = control.text;
	}

	// The value should be checked only when the control is initialized.
	// But even if initialized, the constraints (e.g min or max) might change afterwards, if these constraints depend on other controls.
	// In this case the error must be removed: this is done via the onAcceptableInputChanged which calls the checkValue.
	onInitializedChanged: if (initialized) checkValue(false, false)

	function checkValue(resetLastValidValue, addErrorIfNotFocussed)
	{
		if (!initialized && isBound) return false

		if (control.acceptableInput)
		{
			if (!hasScriptError)
				clearControlError();
			return true;
		}

		if (addErrorIfNotFocussed && activeFocus) return false
		if (!control.validator || (typeof control.validator.validationMessage !== "function")) return false;

		var msg = control.validator.validationMessage(beforeLabel.text)

		if (resetLastValidValue)
		{
			if (textField.useLastValidValue)
				value = textField.lastValidValue
			msg += "<br><br>"
			msg += qsTr("Restoring last correct value: %1").arg(value);
			addControlErrorTemporary(msg)
		}
		else
			addControlError(msg)

		return false
	}



	Rectangle
	{
		id:					beforeLabelRect
		width:				beforeLabel.width
		height:				control.height
		color:				debug ? jaspTheme.debugBackgroundColor : "transparent"
		visible:			beforeLabel.text && textField.visible

		Label
		{
			id:						beforeLabel
			font:					jaspTheme.font
			anchors.verticalCenter: parent.verticalCenter
			color:					enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			text:					textField.label
			width:					implicitWidth
			textFormat:				textField.textFormat
		}
	}

	QTC.TextField
	{
		id:						control
		anchors.left:			beforeLabelRect.visible ? beforeLabelRect.right : parent.left
		anchors.leftMargin:		controlXOffset + (beforeLabelRect.visible ? jaspTheme.labelSpacing : 0)
		width:					textField.fieldWidth //+ (textField.useExternalBorder ? 2 * jaspTheme.jaspControlHighlightWidth : 0)
		font:					jaspTheme.font
		activeFocusOnPress:		textField.editable
		color:					enabled && textField.editable ? jaspTheme.textEnabled : jaspTheme.textDisabled

		padding:				jaspTheme.jaspControlPadding
		leftPadding:			jaspTheme.labelSpacing
		selectByMouse:			true
		focus:					textField.editable
		selectedTextColor:		jaspTheme.white
		selectionColor:			jaspTheme.itemSelectedColor
		enabled:				textField.editable
		// text property is set by TextInpoutBase

		QTC.ToolTip.text		: control.text
		QTC.ToolTip.timeout		: jaspTheme.toolTipTimeout
		QTC.ToolTip.delay		: !hovered ? 0 : jaspTheme.toolTipDelay
		QTC.ToolTip.visible		: contentWidth > width - leftPadding - rightPadding && (hovered || control.activeFocus)

		// The acceptableInput is checked even if the user is still typing in the TextField.
		// In this case, the error should not appear immediately (only when the user is pressing the return key, or going out of focus),
		// so the checkValue is called with addErrorIfNotFocussed set to true: it should not display an error if in focus.
		// In not in focus, the acceptableInput can be changed because another control has changed the constraint of this control: in this case, the error should be displayed.
		onAcceptableInputChanged: checkValue(false, true)

		background: Rectangle
		{
			id:					controlBackground
			color:				jaspTheme.controlBackgroundColor
			border.width:		textField.showBorder && !control.activeFocus	? 1					: 0
			border.color:		jaspTheme.borderColor //If the border width is zero the color is inconsequential
			radius:				jaspTheme.borderRadius
			width:				parent.width   - 2 // (parent+self) border width
			height:				parent.height  - 2
			anchors.centerIn:	parent
		}

		Rectangle
		{
			id:					externalControlBackground
			height:				parent.height + jaspTheme.jaspControlHighlightWidth
			width:				parent.width  + jaspTheme.jaspControlHighlightWidth
			color:				"transparent"
			border.width:		0		//Changed from JASPControl::_setFocusBorder() (like border.color)
			anchors.centerIn:	parent
			opacity:			debug ? .3 : 1
			visible:			textField.useExternalBorder
			radius:				jaspTheme.jaspControlHighlightWidth
		}

		onActiveFocusChanged:
		{
			if (activeFocus)
			{
				if (textField.selectValueOnFocus)
					control.selectAll()
			}
			else
				// When going out of focus, the value must be checked. If the value is wrong, the last valid value should replace the wrong value.
				checkValue(true, false)
		}

		Keys.onReturnPressed: (event)=>
		{
			// When pressing the return key, the value should be checked: if the value is wrong, an error should appear and the focus should stay on this control.
			if (checkValue(false, false))
			{
				var nextItem = nextItemInFocusChain();
				if (nextItem && moveFocusOnEdit)
					nextItem.forceActiveFocus();

				event.accepted = false;
			}
		}

		Keys.onPressed: (event) =>
		{
			var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
			var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );

			if (event.key === Qt.Key_Z && controlPressed)
			{
				if (shiftPressed)
				{
						if (textField.redo())
							event.accepted = true;
				}
				else if (textField.undo())
						event.accepted = true;
			}
		}
	}

	Binding
	{
		// This is a way to set the property height only if fieldHeight is set
		// If not, height should keep its implicit binding.
		target:		control
		property:	"height"
		value:		textField.fieldHeight
		when:		textField.fieldHeight != 0
	}

	Rectangle
	{
		id:					afterLabelRect
		width:				afterLabel.implicitWidth
		height:				control.height
		color:				debug ? jaspTheme.debugBackgroundColor : "transparent"
		visible:			afterLabel.text && textField.visible
		anchors.left:		control.right
		anchors.leftMargin: jaspTheme.labelSpacing
		
		Label
		{
			id:						afterLabel
			font:					jaspTheme.font
			anchors.verticalCenter: parent.verticalCenter
			color:					enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			text:					textField.afterLabel
			textFormat:				textField.textFormat
		}
	}
}
