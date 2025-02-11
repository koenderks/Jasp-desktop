import QtQuick
import QtQuick.Layouts
import JASP.Controls

RoundedButton
{
	property bool	hasSubMenu:			false
	property bool	showHovered:		hasSubMenu ? delayOnhoverTimer.running : hovered
	property color	defaultColor:       "transparent"
	property real	arrowExtraWidth:	arrow.width + jaspTheme.generalAnchorMargin

	id:					root
	font:				jaspTheme.fontRibbon
	color:				(_pressed || activeFocus) ? jaspTheme.buttonColorPressed : (showHovered || selected) ? jaspTheme.buttonColorHovered : defaultColor
	border.width:		0
	centerText:			false
	activeFocusOnTab:	true

	signal hoverClicked();
	onHoverClicked:			forceActiveFocus();

	Timer
	{
		id:					delayOnhoverTimer
		interval:			jaspTheme.hoverTime
		running:			false
		repeat:				false
		onTriggered:		if (hovered && root.hasSubMenu) root.hoverClicked();
    }

	onClicked:				delayOnhoverTimer.stop();
	onHoveredChanged:		if (hasSubMenu)
							{
								if (hovered)	delayOnhoverTimer.start()
								else			delayOnhoverTimer.stop()
							}

	Image
	{
		id:						arrow
		anchors.verticalCenter:	parent.verticalCenter
		anchors.right:			parent.right	
		height:					jaspTheme.subMenuIconHeight
		width:					height
		source:					root.hasSubMenu ? jaspTheme.iconPath + "/large-arrow-right.png" : ""
		visible:				hasSubMenu
		opacity:				enabled ? ((hovered || activeFocus) ? 1 : 0.5) : 0.3
		smooth:					true
		mipmap:					true
		sourceSize.width:		width * 2
		sourceSize.height:		height * 2
	}

}
