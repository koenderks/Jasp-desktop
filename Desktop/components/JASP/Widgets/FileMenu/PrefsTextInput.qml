import QtQuick
import QtQuick.Controls

Rectangle
{
	property alias	text:		textInput.text
	property string	toolTip:	""
	property alias	textInput:	textInput
	property var	nextEl:		null
	
	signal onTextChanged();
	signal editingFinished();

	Component.onCompleted: textInput.editingFinished.connect(editingFinished)
	
	id:					root
	implicitHeight:		textInput.implicitHeight + jaspTheme.generalAnchorMargin * 2
	implicitWidth:		textInput.implicitWidth  + jaspTheme.generalAnchorMargin * 2
	height:				implicitHeight
	width:				implicitWidth
	
	color:				enabled ? jaspTheme.controlBackgroundColor : jaspTheme.controlDisabledBackgroundColor
	border.color:		jaspTheme.buttonBorderColor
	border.width:		1
	onFocusChanged:		if(focus) textInput.focus = true;
	activeFocusOnTab:	true

	ToolTip.visible:	toolTip != "" && toolTipMouseArea.containsMouse
	ToolTip.text:		toolTip
	ToolTip.timeout:	3000
	ToolTip.delay:		500

	MouseArea
	{
		id:					toolTipMouseArea
		hoverEnabled:		toolTip != ""
		acceptedButtons:	Qt.NoButton
		anchors.fill:		parent
	}

	TextInput
	{
		id:					textInput
		text:				"..."
		clip:				true
		font:				jaspTheme.font
		onTextChanged:		root.onTextChanged()
		color:				enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		KeyNavigation.tab:	root.nextEl
		KeyNavigation.down:	root.nextEl
		selectByMouse:		true
		selectedTextColor:	jaspTheme.white
		selectionColor:		jaspTheme.itemSelectedColor


		anchors
		{
			left:			parent.left
			right:			parent.right
			verticalCenter:	parent.verticalCenter
			margins:		jaspTheme.generalAnchorMargin
		}
	}
}
