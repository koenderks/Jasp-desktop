import QtQuick
import QtQuick.Controls
import JASP.Controls

Popup
{
	id:			popupRenameColumnDialog;
	modal:		true;

	y:			(parent.height / 2) - (height / 2)
	x:			(parent.width / 2)  - (width / 2)
	width:		popupLoader.width
	height:		popupLoader.height+1

	property int colIndex;

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		radius:			jaspTheme.borderRadius
	}
	padding:	0

    Connections
	{
		target:					dataSetModel
		function onRenameColumnDialog(columnIndex)
		{
			colIndex = columnIndex;
			popupRenameColumnDialog.open()
		}
    }

	Loader
	{
		id:					popupLoader
		sourceComponent:	visible ? renameComp : null
		visible:			popupRenameColumnDialog.opened
	}

	Component
	{
		id:	renameComp

		Item
		{
			height:	renameButton.y + renameButton.height + jaspTheme.generalAnchorMargin
			width:	200 * jaspTheme.uiScale

			Component.onCompleted:	columnName.forceActiveFocus();

			Text
			{
				id:					title
				text:				qsTr("Rename column")
				font:				jaspTheme.fontGroupTitle
				color:				jaspTheme.textEnabled
				verticalAlignment:	Text.AlignVCenter
				anchors
				{
					top:				parent.top
					topMargin:			jaspTheme.generalAnchorMargin
					horizontalCenter:	parent.horizontalCenter
				}
			}


			TextInput
			{
				id:						columnName
				text:					dataSetModel.columnName(popupRenameColumnDialog.colIndex)
				color:					jaspTheme.textEnabled
				font:					jaspTheme.fontGroupTitle
				selectByMouse:			true
				anchors
				{
					top:				title.bottom
					left:				parent.left
					right:				parent.right
					margins:			jaspTheme.generalAnchorMargin + jaspTheme.jaspControlPadding
				}

				onEditingFinished:		renameButton.clicked();
				Keys.onEnterPressed:	renameButton.clicked();

				KeyNavigation.tab:		renameButton
				KeyNavigation.down:		renameButton

				Rectangle
				{
					color:				jaspTheme.controlBackgroundColor
					border.width:		1
					border.color:		jaspTheme.borderColor
					radius:				jaspTheme.borderRadius
					z:					-1
					anchors.fill:		parent
					anchors.margins:	-jaspTheme.jaspControlPadding
				}

				MouseArea
				{
					acceptedButtons:	Qt.NoButton
					anchors.fill:		parent
					cursorShape:		Qt.IBeamCursor
				}
			}

			RoundedButton
			{
				id:						renameButton
				activeFocusOnTab:		true
				text:					qsTr("Rename")
				onClicked:				{ dataSetModel.setColumnName(popupRenameColumnDialog.colIndex, columnName.text); popupRenameColumnDialog.close(); }
				toolTip:				qsTr("Rename column %1 to %2").arg(popupRenameColumnDialog.colIndex + 1).arg(columnName.text)
				KeyNavigation.right:	closeButtonCross
				KeyNavigation.tab:		closeButtonCross

				anchors
				{
					top:				columnName.bottom
					margins:			jaspTheme.generalAnchorMargin
					topMargin:			jaspTheme.generalAnchorMargin + jaspTheme.jaspControlPadding
					left:				parent.left
					right:				closeButtonCross.left
				}
			}

			RoundedButton
			{
				id:						closeButtonCross
				activeFocusOnTab:		true
				iconSource:				jaspTheme.iconPath + "cross.png"
				width:					height
				height:					renameButton.height
				onClicked:				popupRenameColumnDialog.close()
				toolTip:				qsTr("Close without renaming column")
				KeyNavigation.up:		columnName

				anchors
				{
					right:				parent.right
					top:				renameButton.top
					rightMargin:		jaspTheme.generalAnchorMargin
					bottom:				renameButton.bottom
				}
			}
		}
	}
}
