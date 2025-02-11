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
import JASP.Controls				as JASPControl
import Qt5Compat.GraphicalEffects

FocusScope
{
	id							: menu
	width						: menuRectangle.width
	height						: menuRectangle.height
	visible						: showMe && activeFocus
	x							: Math.min(menuMinPos.x + (menuMinIsMin ? Math.max(0, menuX) : menuX), menuMaxPos.x - (width  + 2) )
	y							: Math.min(menuMinPos.y + (menuMinIsMin ? Math.max(0, menuY) : menuY), menuMaxPos.y - (height + 2) )
	property var	props		: undefined
	property bool	hasIcons	: true
	property real	_iconPad	: 5 * preferencesModel.uiScale
	property int	menuX		: menuOffset.x + menuScroll.x
	property int	menuY		: menuOffset.y + menuScroll.y
	property point	menuOffset	: "0,0"
	property point	menuScroll	: "0,0"
	property point	menuMinPos	: "0,0"
	property point	menuMaxPos	: "0,0"
	property bool	menuMinIsMin: false
	property bool	showMe		: false
	property var    sourceItem  : null
	property point	scrollOri	: "0,0" //Just for other qmls to use as a general storage of the origin of their scrolling

	property int	currentIndex: -1

	onSourceItemChanged: { menu.currentIndex = -1; }

	Keys.onPressed: (event)=>
	{
		switch(event.key)
		{
		case Qt.Key_Up:
		case Qt.Key_Backtab:
			navigate(-1);
			event.accepted = true;
			break;
		case Qt.Key_Down:
		case Qt.Key_Tab:
			navigate(1);
			event.accepted = true;
			break;
		case Qt.Key_Left:
			parentNavigate(-1);
			event.accepted = true;
			break;
		case Qt.Key_Right:
			parentNavigate(1);
			event.accepted = true;
			break;
		case Qt.Key_Return:
		case Qt.Key_Space:
			if (currentIndex > -1)
			{
				callMenuAction(currentIndex)
				menu.currentIndex = -1;
			}
			closeMenu();
			break;
		case Qt.Key_Escape:
			menu.currentIndex = -1;
			closeMenu();
			break;
		default:
			break;
		}
	}

	onPropsChanged:
	{
		hasIcons = (menu.props === undefined || "undefined" === typeof(menu.props["hasIcons"])) ? true : menu.props["hasIcons"]

		if (menu.props === undefined || menu.props["model"] !== resultMenuModel)
			resultsJsInterface.runJavaScript("window.setSelection(false);")
	}

	function toggle(item, props, x_offset = 0, y_offset = 0)
	{
		if (item === menu.sourceItem && menu.visible)
			hide()
		else
			show(item, props, x_offset, y_offset);
	}

	function show(item, props, x_offset = 0, y_offset = 0)
	{
		menu.sourceItem     = item;
		menu.menuMaxPos.x	= Qt.binding(function() { return mainWindowRoot.width;  });
		menu.menuMaxPos.y	= Qt.binding(function() { return mainWindowRoot.height; });
		menu.menuMinPos     = item.mapToItem(null, 1, 1);
		menu.props          = props;
		menu.menuOffset.x	= x_offset;
		menu.menuOffset.y	= y_offset;
		menu.menuScroll		= "0,0";
		menu.showMe			= true;

		menu.forceActiveFocus();

	}

	function hide()
	{
		menu.showMe			= false;
		menu.sourceItem     = null;
		menu.props			= undefined;
		menu.menuMinIsMin	= false;
		menu.menuOffset		= "0,0"
		menu.menuScroll		= "0,0"
		menu.menuMinPos		= "0,0"
		menu.currentIndex   = -1;
	}

	function resizeElements(newWidth)
	{
		for (var i = 0; i < repeater.count; ++i)
			repeater.itemAt(i).width = newWidth;
	}

	function navigate(direction)
	{
		if (menu.props["navigateFunc"] === undefined || typeof(menu.props["navigateFunc"]) === "undefined")
			menu.currentIndex = mod(menu.currentIndex + direction, repeater.count)
		else
			menu.currentIndex = menu.props["navigateFunc"](currentIndex, direction);
	}

	function parentNavigate(direction)
	{
		if (menu.props["parentNavigateFunc"] === undefined || typeof(menu.props["parentNavigateFunc"]) === "undefined")
			return;
		menu.props["parentNavigateFunc"](direction);
	}

	function closeMenu()
	{
		if (menu.sourceItem !== null)
		{
			menu.sourceItem.forceActiveFocus();
			if (menu.sourceItem.myMenuOpen !== undefined && typeof(menu.sourceItem.myMenuOpen) !== 'undefined')
				menu.sourceItem.myMenuOpen = false;
		}
		menu.hide();
	}

	function callMenuAction(index)
	{
		if (menu.sourceItem !== null)
			menu.sourceItem.forceActiveFocus()
		menu.props['functionCall'](index)
	}

	Rectangle
	{
		id		: menuRectangle
		z		: menuShadow.z + 1
		color	: jaspTheme.fileMenuColorBackground
		focus	: true
		width	: column.maxWidth + (itemScrollbar.visible ? itemScrollbar.width : 0)
		height	: menuOffset.y + column.maxHeight > menuMaxPos.y ? menuMaxPos.y - menuOffset.y : column.maxHeight

		MouseArea
		{
			anchors.fill	: parent
			acceptedButtons	: Qt.NoButton
			onWheel			: (wheel)=> { wheel.accepted = true; }
		}

		JASPControl.JASPScrollBar
		{
			id				: itemScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: true
			z				: 1337

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				margins		: 2
			}
		}

		Flickable
		{
			id						: itemFlickable
			anchors.fill			: parent
			anchors.topMargin		: jaspTheme.menuPadding / 2
			anchors.leftMargin		: jaspTheme.menuPadding / 2
			anchors.rightMargin		: itemScrollbar.width + anchors.margins
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: column.width
			contentHeight			: column.height


			Column
			{
				id		: column
				z		: menuRectangle.z + 1
				spacing	: jaspTheme.menuSpacing

				property real maxWidth	: 0
				property real maxHeight	: 0

				Repeater
				{
					id		: repeater
					model	: menu.props === undefined ? undefined : menu.props["model"]

					onItemAdded: (index, item)=>
					{
						if (index === 0)
						{
							column.maxWidth  = 0;
							column.maxHeight = 0;
						}

						column.maxHeight = column.maxHeight + (item.height + jaspTheme.menuSpacing)

						if (index === count - 1)
							column.maxHeight += (jaspTheme.menuPadding - jaspTheme.menuSpacing)

						column.maxWidth = Math.max(item.width + jaspTheme.menuPadding, column.maxWidth);

						if (index === count - 1)
							menu.resizeElements(column.maxWidth);
					}

					delegate: Loader
					{
						sourceComponent :
						{
							if(model.modelData !== undefined)
							{
								if(model.modelData.startsWith("---"))
								{
									if(model.modelData == "---")	return menuSeparator;
									else							return menuGroupTitle;
								}
								return menuDelegate;
							}

							if (model.isSeparator !== undefined && model.isSeparator)			return menuSeparator;
							else if (model.isGroupTitle !== undefined && model.isGroupTitle)	return menuGroupTitle;

							return menuDelegate
						}

						Component
						{
							id: menuDelegate

							Rectangle
							{
								id:		menuItem
								width:	initWidth
								height: jaspTheme.menuItemHeight
								color:	(model.modelData === undefined) && !menuItem.itemEnabled
												? "transparent"
												: mouseArea.pressed || index == currentIndex
													? jaspTheme.buttonColorPressed
													: mouseArea.containsMouse
														? jaspTheme.buttonColorHovered
														: "transparent"

								property bool itemEnabled: menu.props.hasOwnProperty("enabled") ? menu.props["enabled"][index] : (model.modelData !== undefined || model.isEnabled)
								property int padding: 4 + (menu.hasIcons ? 1 : 0) + (menuItemShortcut.text ? 1 : 0)
								property double initWidth: (menu.hasIcons ? menuItemImage.width : 0) + menuItemText.implicitWidth + menuItemShortcut.implicitWidth + menu._iconPad * padding

								Image
								{
									id						: menuItemImage
									height					: menuItem.height - (2 * menu._iconPad)
									width					: menuItem.height - menu._iconPad

									source					: menu.props.hasOwnProperty("icons") ? menu.props["icons"][index] : (model.modelData !== undefined ? "" : menuImageSource)
									smooth					: true
									mipmap					: true
									fillMode				: Image.PreserveAspectFit

									anchors.left			: parent.left
									anchors.leftMargin		: menu._iconPad * 2
									anchors.verticalCenter	: parent.verticalCenter
								}

								Text
								{
									id					: menuItemText
									text				: model.modelData !== undefined ? model.modelData : displayText
									font				: jaspTheme.font
									color				: menuItem.itemEnabled ? jaspTheme.black : jaspTheme.gray
									anchors
									{
										left			: menu.hasIcons ? menuItemImage.right : parent.left
										leftMargin		: menu.hasIcons ? menu._iconPad : menu._iconPad * 2
										verticalCenter	: parent.verticalCenter
									}
								}

								Text
								{
									id					: menuItemShortcut
									text				: menu.props.hasOwnProperty("shortcut") ? menu.props["shortcut"][index] : ""
									font				: jaspTheme.font
									color				: menuItem.itemEnabled ? jaspTheme.black : jaspTheme.gray
									anchors
									{
										right			: parent.right
										rightMargin		: menu._iconPad * 2
										verticalCenter	: parent.verticalCenter
									}
								}
								MouseArea
								{
									id				: mouseArea
									hoverEnabled	: true
									anchors.fill	: parent
									onClicked		: callMenuAction(index)
									enabled			: menuItem.itemEnabled
								}
							}
						}

						Component
						{
							id: menuGroupTitle

							Item
							{
								id		: menuItem
								width	: initWidth
								height	: (isSmall ? 0.5 : 1) * jaspTheme.menuGroupTitleHeight

								property double initWidth: menuItemImage.width + menuItemText.implicitWidth + 15 * preferencesModel.uiScale

								Image
								{
									id					: menuItemImage
									height				: parent.height - (menu._iconPad * 2)
									width				: height

									source				: model.modelData !== undefined ? "" : menuImageSource
									smooth				: true
									mipmap				: true
									fillMode			: Image.PreserveAspectFit
									visible				: source != ""

									anchors
									{
										top				: parent.top
										left			: parent.left
										bottom			: parent.bottom
										leftMargin		: visible ? menu._iconPad : 0
									}
								}

								Text
								{
									id					: menuItemText
									text				: model.modelData !== undefined ? model.modelData.substring(3) : displayText
									font				: jaspTheme.fontGroupTitle
									color				: jaspTheme.textEnabled
									anchors
									{
										left			: menuItemImage.right
										leftMargin		: menu._iconPad
										verticalCenter	: parent.verticalCenter
									}
								}
							}
						}

						Component
						{
							id	: menuSeparator
							ToolSeparator { orientation	: Qt.Horizontal }
						}
					}
				}
			}
		}
	}

	RectangularGlow
	{
		id				: menuShadow
		anchors.fill	: menuRectangle
		color			: jaspTheme.shadow
		spread			: 0.2
		cornerRadius	: menuRectangle.radius + glowRadius
		glowRadius		: 5
	}
}
