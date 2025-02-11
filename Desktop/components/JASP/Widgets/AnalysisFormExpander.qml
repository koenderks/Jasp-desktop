﻿import QtQuick
import QtQuick.Controls
import JASP.Controls
import Qt5Compat.GraphicalEffects

DropArea
{
						id:						analysisFormExpander
						anchors.left:			parent.left
						anchors.right:			parent.right
						height:					expanderButton.height + 1
						keys:					["analysis"]

	property alias		myIndex:				draggableItem.myIndex
	property alias		myAnalysis:				formParent.myAnalysis
	property alias		myForm:					formParent.myForm
	property alias		backgroundFlickable:	formParent.backgroundFlickable

	onEntered: (drag)=>
	{
		if (drag.source.myIndex !== myIndex)
		{
			drag.source.droppedIndex = myIndex
			analysesModel.move(drag.source.myIndex, myIndex)
		}
	}

	function toggleExpander()
	{
		if(analysesModel.currentAnalysisIndex === draggableItem.myIndex)	{ analysesModel.unselectAnalysis(); draggableItem.forceActiveFocus(); }
		else																{ analysesModel.selectAnalysisAtRow(draggableItem.myIndex); }
	}

	function expand(takeFocus = true)
	{
		analysesModel.selectAnalysisAtRow(draggableItem.myIndex);
		if(takeFocus)
			draggableItem.forceActiveFocus();
	}

	Component.onCompleted: myAnalysis.expandAnalysis.connect(toggleExpander)

	Rectangle
	{
		id:						bottomLine
		anchors.bottom:			parent.bottom
		anchors.left:			parent.left
		height:                 1
		width:                  parent.width + 1
		color:                  jaspTheme.buttonBorderColor
		visible:				draggableItem.state != "dragging"
	}

	Item
	{
		id:					draggableItem
		height:				loaderAndError.y
		activeFocusOnTab:	true

		onActiveFocusChanged:	{ if (activeFocus) backgroundFlickable.scrollToElement(expanderButton); }

		property int		myIndex:			-1
		property int		droppedIndex:		-1

		Drag.keys:			["analysis"]
		Drag.active:		mouseArea.drag.active
		Drag.hotSpot.x:		width/2
		Drag.hotSpot.y:		height/2

		states:
		[
			State
			{
				name:	"dragging"
				when:	draggableItem.Drag.active

				ParentChange
				{
					target:			draggableItem
					parent:			backgroundFlickable
				}
				
				AnchorChanges
				{
					target:			draggableItem
					anchors.top:	undefined
					anchors.left:	undefined
					anchors.right:	undefined
				}

				PropertyChanges
				{
					restoreEntryValues: false
					draggableItem
					{
						focus:			true
					}
				}
			},
			
			State
			{
				name:	"chilling"
				when:	!draggableItem.Drag.active

				ParentChange
				{
					target:			draggableItem
					parent:			analysisFormExpander
				}

				AnchorChanges
				{
					target:			draggableItem
					anchors.top:	parent.top
					anchors.left:	parent.left
					anchors.right:	parent.right
				}
			}
		]

		Keys.onPressed: (event) =>
		{
			if (event.key === Qt.Key_Return || event.key === Qt.Key_Space)
			{
				analysisFormExpander.toggleExpander();
			}
		}


		ToolTip
		{
			text:			qsTr("Drag to reorder the analyses")
			timeout:		jaspTheme.toolTipTimeout
			delay:			jaspTheme.toolTipDelay
			font:			jaspTheme.font
			background:		Rectangle { color:	jaspTheme.tooltipBackgroundColor }
			visible:		mouseArea.containsMouse && !analysesModel.moving && analysesModel.rowCount() > 1
			y:				mouseArea.mouseY
			x:				mouseArea.mouseX + 5
		}

		MouseArea
		{
			id:				mouseArea
			onClicked:		{ analysisFormExpander.toggleExpander(); }
			hoverEnabled:	true
			cursorShape:	draggableItem.Drag.active ? Qt.ClosedHandCursor : Qt.PointingHandCursor
			drag.target:	draggableItem

			drag.onActiveChanged:
			{
				if (drag.active)
				{
					analysesModel.unselectAnalysis()
					analysesModel.moving = true
					draggableItem.droppedIndex = -1
				}
				else
				{
					analysesModel.moving = false
					analysesModel.moveAnalysesResults(formParent.myAnalysis, draggableItem.droppedIndex)
				}
			}

			anchors
			{
				top:	parent.top
				left:	parent.left
				right:	parent.right
			}
			height: jaspTheme.formExpanderHeaderHeight + (2 * jaspTheme.formMargin) //We only want to see a tooltip when we are hovering the "button" part of AnalysisFormExpander
		}

		RectangularGlow
		{
			id				: shadow
			anchors.centerIn: draggableItem
			width			: draggableItem.width
			height			: draggableItem.height
			visible			: draggableItem.Drag.active
			color			: jaspTheme.grayDarker
			spread			: 0.2
			cornerRadius	: expanderButton.radius + glowRadius
			glowRadius		: 5
		}

		Rectangle
		{
			// This line appears only when the analysis above this one is dragged.
			anchors
			{
				top:		parent.top
				topMargin:	-1
				left:		parent.left
			}
			height:			1
			width:			parent.width
			color:			jaspTheme.buttonBorderColor
		}

		Rectangle
		{
			id:					expanderButton
			implicitHeight:		loaderAndError.y
			height:				implicitHeight
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			z:					shadow.z + 1
			color:				jaspTheme.uiBackground
			clip:				true

			property bool		expanded:			false
			property bool		shouldExpand:		analysesModel.currentAnalysisIndex == myIndex
			property bool		loadingQml:			!formParent.loaded
			property real		formHeight:			formParent.height
			property bool		firstExpansion:		true //lame but is works

			onExpandedChanged: { if(!shouldExpand) firstExpansion = false; }

			SequentialAnimation {
				id: postExpansionTasksHandler
				PauseAnimation { duration: 100 }
				ScriptAction { script:  expanderButton.postExpansionTasks(10000); }
			}

			function postExpansionTasks(scrollMargin = 0)
			{
				if(typeof backgroundFlickable === 'undefined')
					return;

				expanded = true;
				backgroundFlickable.scrollToElement(expanderButton, scrollMargin);
				if(firstExpansion) //only focus first item on analysis creation
					formParent.nextItemInFocusChain().forceActiveFocus();
				else
					draggableItem.forceActiveFocus();
			}

			Connections {
				target: expanderButton
				enabled: !preferencesModel.animationsOn
				function onImplicitHeightChanged()
				{
					if (expanderButton.shouldExpand && !expanderButton.expanded)
						postExpansionTasksHandler.start();
					else if (!expanderButton.shouldExpand && expanderButton.expanded)
						expanderButton.expanded = false;
				}
			}

			onLoadingQmlChanged:
			{
				if(loadingQml)	qmlLoadingIndicator.startManually();
				else			qmlLoadingIndicator.stopManually();
			}

			Connections
			{
				target:										analysesModel
				function onCurrentAnalysisIndexChanged() {	if(analysesModel.currentAnalysisIndex == draggableItem.myIndex) analysesModel.currentFormHeight = Qt.binding(function(){ return expanderButton.formHeight; }); }
			}

			states: [
				State {	name: "expanded";	when: expanderButton.shouldExpand && !expanderButton.loadingQml ;	PropertyChanges {	target: expanderButton;		implicitHeight: loaderAndError.y + loaderAndError.implicitHeight;											}	},
				State { name: "loading";	when: expanderButton.shouldExpand &&  expanderButton.loadingQml ;	PropertyChanges {	target: expanderButton;		implicitHeight: qmlLoadingIndicator.y + qmlLoadingIndicator.implicitHeight;									}	},
				State { name: "imploded";	when: !expanderButton.shouldExpand;									PropertyChanges {	target: expanderButton;		implicitHeight: loaderAndError.y;																			}	}
			]

			transitions: Transition
			{
				enabled:	preferencesModel.animationsOn
				reversible:	true

				onRunningChanged:
				{
					if (expanderButton.shouldExpand)
						Qt.callLater(expanderButton.postExpansionTasks);
					else
						expanderButton.expanded = false;
				} //expansion ended

				// Do not use a behavior here: this would interfere with the animation of the ExpanderButtons in the form
				NumberAnimation		{ property: "implicitHeight";	duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
			}

			FocusScope
			{
				id:				expanderRectangle
				height:			jaspTheme.formExpanderHeaderHeight  //label.contentHeight
				scale:			mouseArea.containsMouse && !expanderButton.expanded ? 1.015 : 1.0

				onActiveFocusChanged:	{ if (activeFocus) backgroundFlickable.scrollToElement(expanderButton); }

				anchors
				{
					left:		parent.left
					right:		parent.right
					top:		parent.top
					topMargin:	jaspTheme.formMargin
				}

				Image
				{
					id:					expanderIcon
					anchors
					{
						left:			parent.left
						leftMargin:		10 * preferencesModel.uiScale
						verticalCenter:	parent.verticalCenter
					}
					rotation:		expanderButton.expanded ? 90 : 0
					height:			analysisTitle.height * 0.88 //expanderRectangle.height / 1.5
					width:			height
					source:			draggableItem.activeFocus ? jaspTheme.iconPath + "/large-arrow-right-selected.png" : jaspTheme.iconPath + "/large-arrow-right.png"
					sourceSize
					{
						width:	expanderIcon.width * 2
						height:	expanderIcon.height * 2
					}

					Behavior on rotation { enabled: preferencesModel.animationsOn; RotationAnimation { duration: 200 } }

				}

				Item
				{
					id:			analysisTitleItem
					height:		analysisTitle.height

					anchors
					{
						left:			expanderIcon.right
						right:			rSyntaxButton.left
						leftMargin:		expanderIcon.anchors.leftMargin
						rightMargin:	2 * preferencesModel.uiScale
						verticalCenter:	parent.verticalCenter
					}

					Text
					{
						id:				analysisTitle
						text:			formParent.myForm ? formParent.myForm.title : "?"
						font:			jaspTheme.fontLabel
						color:			jaspTheme.textEnabled
						visible:		!analysisTitleInput.visible
						elide:			Text.ElideMiddle

						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
						}
					}

					TextInput
					{
						id:					analysisTitleInput
						font:				jaspTheme.fontLabel
						visible:			false
						selectByMouse:		true
						color:				jaspTheme.grayDarker
						clip:				true

						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
						}


						Keys.onEscapePressed: 	stopEditing(false);
						Keys.onEnterPressed:	stopEditing(true);
						Keys.onReturnPressed: (event)=> 	stopEditing(true);
						onActiveFocusChanged:	if(!activeFocus && visible)	stopEditing(true);

						function startEditing()
						{
							text	= analysisTitle.text;
							visible = true;

							forceActiveFocus();
						}

						function stopEditing(storeChangedValue)
						{
							if(storeChangedValue && formParent.myForm)
								formParent.myForm.title = text;

							visible = false;
						}
					}
				}

				MenuButton
				{
					id:					rSyntaxButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "/R-roundbutton.svg" :  jaspTheme.iconPath + "/R-roundbutton-disabled.svg"
					enabled:			expanderButton.expanded
					onClicked:			if (formParent.myForm) formParent.myForm.toggleRSyntax();
					toolTip:			preferencesModel.showRSyntax ? qsTr("Hide R Syntax") : qsTr("Show R syntax")
					radius:				height
					opacity:			editButton.opacity
					visible:            formParent.myForm && formParent.myForm.showRButton
					anchors
					{
						top:			parent.top
						right:			editButton.left
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}

				MenuButton
				{
					id:					editButton
					width:				height
					iconSource:			jaspTheme.iconPath + "/edit-pencil.png" // Icon made by Chanut from https://www.flaticon.com/
					enabled:			expanderButton.expanded
					onClicked:			analysisTitleInput.startEditing();
					toolTip:			qsTr("Edit the title of this analysis")
					radius:				height
					opacity:			enabled ? 1 : 0.1
					anchors
					{
						top:			parent.top
						right:			copyButton.left
						bottom:			parent.bottom
						topMargin:		4 * preferencesModel.uiScale
						bottomMargin:	4 * preferencesModel.uiScale
					}
				}

				MenuButton
				{
					id:					copyButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "/duplicate.svg" : jaspTheme.iconPath + "/duplicate_disabled.svg"
					enabled:			expanderButton.expanded
					onClicked:			analysisFormExpander.myAnalysis.duplicateMe()
					toolTip:			qsTr("Duplicate this analysis")
					radius:				height
					opacity:			editButton.opacity
					anchors
					{
						top:			parent.top
						right:			helpButton.left
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}

				MenuButton
				{
					id:					helpButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "info-button.png" : jaspTheme.iconPath + "info-button-black.png" // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
					opacity:			editButton.opacity
					//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
					enabled:			expanderButton.expanded
					onClicked:			if(preferencesModel.generateMarkdown || !helpModel.pageExists(formParent.myAnalysis.helpFile()))
										{
											if(formParent.myForm && helpModel.markdown !== formParent.myForm.helpMD)
											{
												helpModel.analysis	= formParent.myAnalysis;
												helpModel.markdown  = Qt.binding(function(){ return formParent.myForm.helpMD; });
											}
											else
											{
												helpModel.visible  = false;
												helpModel.markdown = ""; //break binding
												helpModel.analysis = null
											}
											
												
										}
										else
										{
											helpModel.markdown = "";
											helpModel.showOrTogglePageForAnalysis(formParent.myAnalysis)
										}
										
					toolTip:			qsTr("Show info for this analysis")
					radius:				height
					anchors
					{
						top:			parent.top
						right:			closeButton.left
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
					}
				}

				MenuButton
				{
					id:					closeButton
					width:				height
					iconSource:			enabled ? jaspTheme.iconPath + "close-button.png" : jaspTheme.iconPath + "close-button-black.png" // {close-button, close-button-grey}.png Icons made by Smashicons from https://www.flaticon.com/
					opacity:			editButton.opacity
					//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
					enabled:			expanderButton.expanded
					onClicked:			analysesModel.removeAnalysis(formParent.myAnalysis)
					toolTip:			qsTr("Remove this analysis")
					radius:				height
					anchors
					{
						top:			parent.top
						right:			parent.right
						bottom:			parent.bottom
						topMargin:		editButton.anchors.topMargin
						bottomMargin:	editButton.anchors.bottomMargin
						rightMargin:	4 * preferencesModel.uiScale
					}
				}
			}

			LoadingIndicator
			{

				id:						qmlLoadingIndicator
				visible:				expanderButton.loadingQml && expanderButton.expanded && formParent.error == ""
				implicitHeight:			300 * jaspTheme.uiScale
				autoStartOnVisibility:	false

				anchors
				{
					top:				expanderRectangle.bottom
					left:				parent.left
					right:				parent.right
					margins:			jaspTheme.formMargin
				}

			}

			Item
			{
				id:					loaderAndError
				implicitHeight:		formParent.loaded ? formParent.height : formParent.error != "" ? errorRect.height * preferencesModel.uiScale : 0
				visible:			expanderButton.expanded && (formParent.loaded || formParent.error != "")

				anchors
				{
					top:				expanderRectangle.bottom
					left:				parent.left
					right:				parent.right
					margins:			jaspTheme.formMargin
				}

				Rectangle
				{
					id:				errorRect
					visible:		formParent.error != ""
					anchors.top:	parent.top
					color:			jaspTheme.errorMessagesBackgroundColor
					width:			jaspTheme.formWidth - ( 2 * jaspTheme.formMargin )
					height:			visible ? errorMessagesText.height : 0

					Text
					{
						id:					errorMessagesText
						anchors.centerIn:	parent
						width:				parent.width
						padding:			5
						verticalAlignment:	Text.AlignVCenter
						text:				formParent.error
						wrapMode:			Text.Wrap
						
						//onTextChanged:		messages.log("errorMessagesText text changed to '" + text + "'");
					}
				}

				Item
				{
					id:					formParent
					height:				myForm ? myForm.implicitHeight : 0

					property string error:		!myAnalysis ? "No Analysis!\n" : myAnalysis.qmlError
					property bool	loaded:		myForm

					anchors
					{
						top:			errorRect.bottom
						topMargin:		errorRect.visible ? jaspTheme.formMargin : 0
						left:			parent.left
						right:			parent.right
					}

					property var	myForm:					myAnalysis ? myAnalysis.formItem : null
					property var	myAnalysis:				null	///< Set from AnalysisForms.qml and given through Analyses or `analysesModel`
					property var	backgroundFlickable:	null	///< Set from AnalysisForms.qml

					onMyFormChanged:
					{
						if(myForm != null)
						{
							myForm.backgroundForms = backgroundFlickable;
							myForm.customMenu = customMenu
						}
					}

					onMyAnalysisChanged:
					{
						if(myAnalysis)
							myAnalysis.createForm(formParent); //Make sure Analysis knows where to create the form (and might even trigger the creation immediately)
					}

					Connections
					{
						target: myForm
						function onActiveJASPControlChanged()
						{
							if (!myForm || !myForm.activeJASPControl)
								return;

							const control = myForm.activeJASPControl;
							if (control.focusReason !== Qt.MouseFocusReason)
								backgroundFlickable.scrollToElement(control, 50 * jaspTheme.uiScale);
						}
					}
				}
			}
		}
	}
}
