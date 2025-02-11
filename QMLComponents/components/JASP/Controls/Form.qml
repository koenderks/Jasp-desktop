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
import QtQuick.Controls as QtC
import JASP.Controls
import JASP

AnalysisForm
{
	id					: form
	implicitWidth		: jaspTheme.formWidth - ( 2 * jaspTheme.formMargin )
	implicitHeight		: formContent.height + (jaspTheme.formMargin * 2)
	width				: implicitWidth
	height				: implicitHeight
	//errorMessagesItem	: errorMessagesBox
	
	default property alias	content		: contentArea.children
	property alias	form				: form
	property alias	jaspForm			: form
	property alias	jaspAnalysis		: form.analysis
	property alias	_contentArea		: contentArea
	property real	minimumYMsgs		: warningMessagesBox.height + warningMessagesBox.y
	property int	majorVersion		: 1
	property int	minorVersion		: 0
	property int	availableWidth		: form.width - 2 * jaspTheme.formMargin
	property var	backgroundForms		: undefined
	property var	customMenu			: undefined
	property alias	columns				: contentArea.columns
	property bool	runAnalysisWhenOptionChange : true

	property int    plotHeight			: 320
	property int    plotWidth			: 480


	Keys.onPressed: (event) => { event.accepted = true; } //dont let leftover input propagate upwards

	ALTNavigation.enabled:			visible
	ALTNavigation.scopeOnly:		true
	ALTNavigation.parent:			null //default root
					
	MouseArea
	{
		z:				-5
		anchors.fill:	parent
		onClicked:		form.forceActiveFocus()
	}

	IntegerField { visible: false; name: "plotWidth";  value: plotWidth }
	IntegerField { visible: false; name: "plotHeight"; value: plotHeight }
	
	
	FocusScope
	{
		id:				formContent
		width:			parent.width
		height:			oldFileMessagesBox.height + errorMessagesBox.height + warningMessagesBox.height + rSyntaxElement.height + contentArea.implicitHeight
		anchors
		{
			top:		form.top
			left:		form.left
		}

		Rectangle
		{
			id:				oldFileMessagesBox
			visible:		needsRefresh && form.hasVolatileNotes //Ill leave the text as is for now to avoid having to go back to the po again
			color:			jaspTheme.controlWarningBackgroundColor
			width:			form.implicitWidth
			height:			visible ? oldAnalysisText.height : 0
			anchors.top:	parent.top
			radius:			jaspTheme.borderRadius

			Text
			{
				id:					oldAnalysisText
				color:				jaspTheme.controlWarningTextColor
				anchors.centerIn:	parent
				padding:			5 * jaspTheme.uiScale
				wrapMode:			Text.Wrap
				width:				parent.width - 10 * jaspTheme.uiScale
				verticalAlignment:	Text.AlignVCenter
				text:				qsTr("This analysis was created with an older version of JASP (or a dynamic module)") + //I do not want to bother with formatting strings here to be honest
									( !hasVolatileNotes ?
										qsTr(", refreshing could give a slightly different result.") :
										qsTr(", to keep your notes where they are it is highly recommended to first refresh your analyses!"))

			}
		}
				
		Rectangle
		{
			id:					errorMessagesBox
			visible:			form.errors !== ""
			color:				jaspTheme.controlErrorBackgroundColor
			width:				form.implicitWidth
			height:				visible ? errorMessagesText.height : 0
			anchors.top:		oldFileMessagesBox.bottom
			anchors.margins:	visible ? jaspTheme.generalAnchorMargin : 0
			radius:				jaspTheme.borderRadius

			Text
			{
				id:					errorMessagesText
				anchors.centerIn:	parent
				padding:			5 * jaspTheme.uiScale
				wrapMode:			Text.Wrap
				width:				parent.width - 10 * jaspTheme.uiScale
				verticalAlignment:	Text.AlignVCenter
				text:				form.errors
				textFormat:			Text.StyledText
				color:				jaspTheme.controlErrorTextColor
				linkColor:			jaspTheme.controlErrorTextColor
				onLinkActivated:	Qt.openUrlExternally(link)

				MouseArea
				{
					anchors.fill:		parent
					z:					20
					acceptedButtons:	Qt.NoButton
					cursorShape:		parent.hoveredLink ? Qt.PointingHandCursor : Qt.ArrowCursor
				}
			}

			CrossButton { onCrossClicked: form.clearFormErrors() }
		}



		Rectangle
		{
			id:					warningMessagesBox
			visible:			form.warnings !== ""
			color:				jaspTheme.controlWarningBackgroundColor
			width:				form.implicitWidth
			height:				visible ? warningMessagesText.height : 0
			anchors.top:		errorMessagesBox.bottom
			anchors.margins:	visible ? jaspTheme.generalAnchorMargin : 0
			radius:				jaspTheme.borderRadius

			Text
			{
				id:					warningMessagesText
				anchors.centerIn:	parent
				padding:			5 * jaspTheme.uiScale
				wrapMode:			Text.Wrap
				width:				parent.width - 10 * jaspTheme.uiScale
				verticalAlignment:	Text.AlignVCenter
				text:				form.warnings
				onLinkActivated:	Qt.openUrlExternally(link)
				textFormat:			Text.StyledText
				color:				jaspTheme.controlWarningTextColor
				linkColor:			jaspTheme.controlWarningTextColor

				MouseArea
				{
					anchors.fill:		parent
					z:					20
					acceptedButtons:	Qt.NoButton
					cursorShape:		parent.hoveredLink ? Qt.PointingHandCursor : Qt.ArrowCursor
				}
			}

			CrossButton { onCrossClicked: form.clearFormWarnings(); warning: true; }
		}

		Item
		{
			id:					rSyntaxElement
			anchors.top:		warningMessagesBox.bottom
			width:				parent.width
			height:				visible ? rScriptArea.y + rScriptArea.height : 0
			visible:			preferencesModel.showRSyntax && showRButton

			Button
			{
				id:					generateWrapperButton
				visible:            DEBUG_MODE || form.developerMode
				height:				visible ? implicitHeight : 0
				label:				qsTr("Generate Wrapper")
				onClicked:			popup.open()

				QtC.Popup
				{
					id: popup

					parent:				QtC.Overlay.overlay
					anchors.centerIn:	parent

					width:	400  * jaspTheme.uiScale
					height: 600  * jaspTheme.uiScale

					modal:	true

					TextArea
					{
						anchors.fill:				parent
						isBound:					false
						applyScriptInfo:			""
						control.readOnly:			true
						control.selectByKeyboard:	true
						onVisibleChanged:			if (visible) 	text = form.generateWrapper()
					}
				}
			}

			CheckBox
			{
				id:					showAllROptionsCheckBox
				anchors.top:		generateWrapperButton.visible ? undefined : rSyntaxElement.top
				anchors.bottom:		generateWrapperButton.visible ? generateWrapperButton.bottom : undefined
				anchors.right:		rSyntaxElement.right
				label:				qsTr("Show all options")
				isBound:			false
				checked:			showAllROptions
				onClicked:			setShowAllROptions(!showAllROptions)
			}

			TextArea
			{
				id:					rScriptArea
				name:				form.rSyntaxControlName

				anchors.top:		showAllROptionsCheckBox.bottom
				anchors.topMargin:	jaspTheme.generalAnchorMargin
				width:				parent.width
				height:				visible ? 100 * preferencesModel.uiScale : 0
				text:				form.rSyntaxText
				textType:           JASPControl.TextTypeRcode
				isBound:			false
				onApplyRequest:		form.sendRSyntax(text)
				checkSyntax:		false

				onInitializedChanged: if (preferencesModel.showRSyntax) control.forceActiveFocus() // If the textarea has already some large text, then it does not display it if it does not get temporarly the focus...
			}
		}

		GridLayout
		{
			id:					contentArea
			anchors.top:		rSyntaxElement.bottom
			anchors.margins:	warningMessagesBox.visible || errorMessagesBox.visible || rSyntaxElement.visible ? jaspTheme.generalAnchorMargin : 0
			width:				form.implicitWidth
		}
	}
	
	Component.onCompleted:	formCompletedSignal();
}
