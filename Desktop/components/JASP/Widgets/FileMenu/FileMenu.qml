import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import JASP.Controls
import JASP
import FileOperation

FocusScope
{
	id: fileMenu

	Keys.onEscapePressed:
	{
		ribbon.focus			 = true;
		ribbon.isFileMenuPressed = true;  // Close FileMenu but show the button as pressed
		fileMenuModel.visible	= false;
	}
	Keys.onDownPressed:		fileMenuModel.actionButtons.selectButtonDown();
	Keys.onUpPressed:		fileMenuModel.actionButtons.selectButtonUp();
	Keys.onTabPressed:		fileMenuModel.actionButtons.selectButtonDown();
	Keys.onBacktabPressed:  fileMenuModel.actionButtons.selectButtonUp();

	function showToolSeperator(typeRole)
	{
		return typeRole === FileOperation.Close || typeRole === FileOperation.Preferences || typeRole === FileOperation.About
	}

	width:		slidePart.width
	height:		600
	z:			1
	visible:	fileMenuAnimation.running ? actionMenu.x + actionMenu.width > 0 : fileMenuModel.visible

	property int  actionButtionHeight:		35 * preferencesModel.uiScale
	property int  resourceButtonHeight:		1.5 * actionButtionHeight


	Connections
	{
		target:				fileMenuModel
		function onVisibleChanged()
		{
			if (fileMenuModel.visible)
				actionMenu.forceActiveFocus();
		}
	}

	Item
	{
		id:		slidePart

		property real desiredX: !fileMenuModel.visible ? -(resourceScreen.otherColumnsWidth + resourceScreen.width) : 0

		x:		desiredX
		width:	(actionMenu.width + resourceMenu.width ) + (resourceScreen.aButtonVisible ? resourceScreen.width : 0)
		height: fileMenu.height

		Behavior on x
		{
			enabled:		preferencesModel.animationsOn

			PropertyAnimation
			{
				id:				fileMenuAnimation
				duration:		jaspTheme.fileMenuSlideDuration
				easing.type:	Easing.OutCubic
			}
		}

		Rectangle
		{
			z:				3
			color:			jaspTheme.fileMenuColorBackground
			border.width:	1
			border.color:	jaspTheme.uiBorder
			anchors.fill:	actionMenu
		}

		Flickable
		{
			id:				actionMenu
			anchors.left:	parent.left
			width:			fileMenuModel.actionButtons.width + jaspTheme.subMenuIconHeight + jaspTheme.generalAnchorMargin * 5
			height:			parent.height
			z:				4
			contentWidth:	width
			contentHeight:	fileAction.implicitHeight

			ALTNavigation.enabled:		true
			ALTNavigation.parent:		ribbon.fileMenuButton
			ALTNavigation.scopeOnly:	true
			ALTNavigation.foreground:	fileMenuModel.visible

			Column
			{
				id:			fileAction
				spacing:	jaspTheme.menuSpacing

				anchors
				{
					fill:		parent
					margins:	jaspTheme.generalAnchorMargin
				}

				Repeater
				{
					id:		actionRepeaterId
					model:	fileMenuModel.actionButtons

					Item
					{
						id:				itemActionMenu
						width:			fileMenuModel.actionButtons.width + jaspTheme.subMenuIconHeight + jaspTheme.generalAnchorMargin + jaspTheme.generalAnchorMargin
						anchors.left:	parent.left
						height:			actionButtionHeight + actionToolSeperator.height
						enabled:		enabledRole

						MenuButton
						{
							id:					actionMenuButton
							hasSubMenu:			hasSubMenuRole
							width:				parent.width
							height:				actionButtionHeight
							text:				nameRole
							selected:			selectedRole
							focus:				selectedRole

							ALTNavigation.enabled:		true
							ALTNavigation.x:			width * 0.9
							ALTNavigation.onTagMatch:	{ clicked(); }

							Keys.onLeftPressed:
							{
								fileMenuModel.visible    = false;
								ribbon.isFileMenuPressed = true;
								ribbon.focus             = true;
							}
							Keys.onRightPressed:	if(hasSubMenuRole) resourceMenu.forceActiveFocus()
							Keys.onDownPressed:		fileMenuModel.actionButtons.selectButtonDown()
							Keys.onUpPressed:		fileMenuModel.actionButtons.selectButtonUp()
							onHoverClicked:
							{
								actionMenuButton.forceActiveFocus();
								fileMenuModel.actionButtons.buttonClicked(typeRole);
							}

							onClicked:
							{
								hoverClicked();
								if(hasSubMenuRole)
									resourceMenu.forceActiveFocus()
							}
						}

						ToolSeparator
						{
							id:					actionToolSeperator
							anchors
							{
								top:			actionMenuButton.bottom
								topMargin:		(showToolSeperator(typeRole) ? 3 : 0)  * preferencesModel.uiScale
								left:			parent.left
								right:			parent.right
								leftMargin:		-jaspTheme.generalAnchorMargin
								rightMargin:	-jaspTheme.generalAnchorMargin
							}

							orientation:		Qt.Horizontal
							visible:			showToolSeperator(typeRole)
						}
					}
				}
			}
		}

		Rectangle
		{
			z:				1
			color:			jaspTheme.fileMenuColorBackground
			border.width:	1
			border.color:	jaspTheme.uiBorder
			anchors.fill:	resourceMenu
		}


		Flickable
		{
			id:					resourceMenu

			width:				!hasButtons ? 0 : fileMenuModel.resourceButtons.width + jaspTheme.subMenuIconHeight + jaspTheme.generalAnchorMargin * 5
			height:				parent.height
			anchors.left:		actionMenu.right
			anchors.leftMargin: hasButtons ? 0 : -width
			z:					2
			contentWidth:		width
			contentHeight:		resourceLocation.implicitHeight

			onFocusChanged:		if(focus) fileMenuModel.resourceButtons.selectFirstButtonIfNoneSelected();

			Keys.onLeftPressed:		actionMenu.forceActiveFocus();
			Keys.onEscapePressed:	actionMenu.forceActiveFocus();
			
			
			
			property bool hasButtons: resourceRepeaterId.count > 0

			visible: hasButtons

			Behavior on anchors.leftMargin
			{
				enabled:		preferencesModel.animationsOn

				PropertyAnimation
				{
					id:				resourceMenuAnimation
					duration:		jaspTheme.fileMenuSlideDuration
					easing.type:	Easing.OutCubic
				}
			}

			Column
			{
				id:							resourceLocation

				anchors
				{
					fill:		parent
					margins:	jaspTheme.generalAnchorMargin
				}
				

				spacing:					6 * preferencesModel.uiScale

				Repeater
				{
					id:		resourceRepeaterId
					model:	fileMenuModel.resourceButtonsVisible

					Item
					{

						id:					itemResourceMenu
						width:				parent.width
						height:				resourceButtonHeight
						enabled:			enabledRole

						MenuButton
						{
							id:						resourceButton
							hasSubMenu:				true
							width:					parent.width
							height:					parent.height
							anchors.left:			parent.left
							enabled:				!resourceMenuAnimation.running

							text:					nameRole
							selected:				selectedRole
							focus:					selectedRole

							Keys.onLeftPressed:		actionMenu.forceActiveFocus();
							Keys.onRightPressed:	showSelectedSubScreen.forceActiveFocus()
							Keys.onDownPressed:		fileMenuModel.resourceButtons.selectButtonDown()
							Keys.onUpPressed:		fileMenuModel.resourceButtons.selectButtonUp()
							Keys.onTabPressed:		fileMenuModel.resourceButtons.selectButtonDown()
							Keys.onBacktabPressed: 	fileMenuModel.resourceButtons.selectButtonUp()
							onHoverClicked:			fileMenuModel.resourceButtons.selectedButton = typeRole
							onClicked:
							{
								hoverClicked();
								showSelectedSubScreen.forceActiveFocus();
							}
						}

					}
				}
			}
		}

		Item
		{
			id:			dropShadow

			y:			0
			x:			resourceScreen.x + resourceScreen.width
			height:		resourceScreen.height
			width:		jaspTheme.shadowRadius

			visible:	resourceScreen.visible
			z:			-3

			Rectangle
			{
				anchors.centerIn:	parent
				height:				dropShadow.width
				width:				dropShadow.height

				rotation: -90
				gradient: Gradient
				{
					GradientStop	{ position: 0.0; color: jaspTheme.shadow; }
					GradientStop	{ position: 1.0; color: "transparent" }
				}
			}
		}

		Rectangle
		{
			property real otherColumnsWidth:	actionMenu.width + resourceMenu.width
			property bool aButtonVisible:		resourceRepeaterId.count > 0 && fileMenuModel.resourceButtons.currentQML !== ''

			property real desiredWidth:			Math.min(mainWindowRoot.width - otherColumnsWidth, 600 * preferencesModel.uiScale)
			property real desiredX:				otherColumnsWidth - (aButtonVisible ? 0 : desiredWidth)

			property string previousQML: ""
			property string currentQML: ""

			id:				resourceScreen

			x:				desiredX
			width:			desiredWidth
			height:			parent.height

			border.width:	1
			border.color:	jaspTheme.grayDarker
			color:			jaspTheme.fileMenuColorBackground
			z:				-2

			Behavior on x
			{
				enabled:		preferencesModel.animationsOn

				PropertyAnimation
				{
					id:				fileMenuResourceLoaderAnimation
					duration:		jaspTheme.fileMenuSlideDuration
					easing.type:	Easing.OutCubic
				}
			}

			onXChanged:
				if(resourceScreen.x === resourceScreen.desiredX && resourceScreen.currentQML === "" && resourceScreen.previousQML !== "")
					resourceScreen.previousQML = "";

			Connections
			{
				target:	fileMenuModel.resourceButtons
				function onCurrentQMLChanged(currentQML)
				{
					resourceScreen.previousQML = resourceScreen.currentQML
					resourceScreen.currentQML  = currentQML
				}
			}

			Loader
			{
				id:						showSelectedSubScreen
				anchors.fill:			parent
				source:					resourceScreen.currentQML === "" && resourceScreen.x > resourceScreen.desiredX ? resourceScreen.previousQML : resourceScreen.currentQML
				Keys.onLeftPressed:		resourceMenu.forceActiveFocus()
				Keys.onEscapePressed:	resourceMenu.forceActiveFocus()
			}
		}

		MouseArea
		{
			id:					gottaCatchEmAll //Clicks that is
			anchors.fill:		parent
			z:					-6
			onWheel:			(wheel)=>{ wheel.accepted = true; }
		}
	}
}
