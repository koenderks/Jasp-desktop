import QtQuick
import JASP.Widgets
import QtQuick.Controls

//Only works vertically right now...

Rectangle
{
	id:				handleRoot

	signal arrowClicked
	signal handleDragging(bool active, var mouseArea)
	property bool pointingLeft: true
	property bool showArrow:	true
	property bool dragEnabled:	true
	property bool removeLeftBorder: false

	property string toolTipDrag:	""
	property string toolTipArrow:	""
	property bool	hovered:		hoverMouse.containsMouse
	property alias	dragging:		hoverMouse.drag.active
	property alias	dragX:			hoverMouse.x


	width:			jaspTheme.splitHandleWidth
	anchors
	{
		top:			parent.top
		bottom:			parent.bottom
		topMargin:		-1
		bottomMargin:	-1
		leftMargin:		removeLeftBorder ? -1 : 0
	}
	color:			handleRoot.dragEnabled && handleRoot.hovered ? jaspTheme.grayLighter : jaspTheme.uiBackground
	border.color:	jaspTheme.uiBorder
	border.width:	1

	Drag.active: hoverMouse.drag.active


	ToolTip
	{
		text:			handleRoot.toolTipDrag
		timeout:		jaspTheme.toolTipTimeout
		delay:			jaspTheme.toolTipDelay
		font:			jaspTheme.font
		background:		Rectangle { color:	jaspTheme.tooltipBackgroundColor }
		visible:		handleRoot.dragEnabled && hoverMouse.containsMouse && handleRoot.toolTipDrag !== ""
		y:				hoverMouse.mouseY + 10
		x:				parent.width / 2
	}

	MouseArea
	{
		id:					hoverMouse
		//acceptedButtons:	Qt.NoButton
		hoverEnabled:		true
		z:					-20
		anchors
		{
			fill:			parent
			leftMargin:		-1 //this makes sure we're overlapping all of the Splitview and don't get the Qt::SplitHCursor when there is no data
			rightMargin:	-1
		}
		cursorShape:		handleRoot.dragEnabled ? Qt.SplitHCursor : Qt.ArrowCursor //Take into account resizing? styleData.resizing
		//onPositionChanged:	(mouse)=>{ mouse.accepted = true; }

		drag.target: parent
		drag.axis: Drag.XAxis
		drag.onActiveChanged: handleDragging(drag.active, hoverMouse)
	}

	Item
	{
		id:			centerElement
		height:		160
		anchors
		{
			verticalCenter: parent.verticalCenter
			left:			parent.left
			leftMargin:		1
			right:			parent.right
			rightMargin:	1
		}

		Loader //No arrow? then three dots in the center instead
		{
			sourceComponent:	handleRoot.showArrow ? undefined : threeDotsComp
			anchors.centerIn:	parent
		}

		Rectangle
		{

			color:			arrowMouse.containsMouse ? jaspTheme.grayLighter : jaspTheme.uiBackground
			visible:		handleRoot.showArrow
			anchors.fill:	parent

			MouseArea
			{
				id:				arrowMouse
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		handleRoot.arrowClicked()
				z:				3
			}

			ToolTip
			{
				text:			handleRoot.toolTipArrow
				timeout:		jaspTheme.toolTipTimeout
				delay:			jaspTheme.toolTipDelay
				font:			jaspTheme.font
				background:		Rectangle { color:	jaspTheme.tooltipBackgroundColor }
				visible:		handleRoot.toolTipArrow !== "" && arrowMouse.containsMouse
				y:				arrowMouse.mouseY + 15
				x:				parent.width / 2
			}

			Image
			{

				readonly property string iconsFolder:		jaspTheme.iconPath + ""
				readonly property string leftIcon:			"arrow-left.png"
				readonly property string rightIcon:			"arrow-right.png"

				source:					iconsFolder + (handleRoot.pointingLeft ? leftIcon : rightIcon)
				width:					parent.width - (4 * preferencesModel.uiScale)
				height:					width
				sourceSize.width:		width  * 2;
				sourceSize.height:		height * 2;

				anchors.centerIn:		parent

			}
		}
	}

	Item
	{
		anchors
		{
			top:	parent.top
			bottom:	centerElement.top
			left:	parent.left
			right:	parent.right
		}

		Loader
		{
			visible:			height < parent.height
			sourceComponent:	threeDotsComp
			anchors.centerIn:	parent
		}

	}

	Item
	{
		anchors
		{
			top:	centerElement.bottom
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}

		Loader
		{
			visible:			height < parent.height
			sourceComponent:	threeDotsComp
			anchors.centerIn:	parent
		}

	}

	Component
	{
		id:		threeDotsComp

		Item
		{
							id:			threeDots
							height:		width * 4
							width:		jaspTheme.splitHandleWidth * 0.3
			property color	kleur:		jaspTheme.grayDarker
							visible:	handleRoot.dragEnabled

			Rectangle
			{
				color:		threeDots.kleur
				height:		width
				radius:		width

				anchors
				{
					top:	parent.top
					left:	parent.left
					right:	parent.right
				}
			}

			Rectangle
			{
				color:				threeDots.kleur
				height:				width
				radius:				width
				anchors
				{
					verticalCenter:	parent.verticalCenter
					left:			parent.left
					right:			parent.right
				}
			}

			Rectangle
			{
				color:		threeDots.kleur
				height:		width
				radius:		width

				anchors
				{
					left:	parent.left
					right:	parent.right
					bottom:	parent.bottom
				}
			}
		}
	}
}
