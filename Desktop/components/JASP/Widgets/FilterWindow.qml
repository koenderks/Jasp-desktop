import QtQuick
import QtQuick.Controls
import JASP.Controls	as JaspControls
import "FilterConstructor"
import JASP

FocusScope
{
	id:							filterContainer
	visible:					opened

	property bool	opened:					false
	property int	minimumHeightTextBoxes: 50 * preferencesModel.uiScale
	property bool	showEasyFilter:			true
	property int	desiredMinimumHeight:	easyFilterConstructor.desiredMinimumHeight
	property int	desiredHeight:			easyFilterConstructor.desiredHeight
	
	onShowEasyFilterChanged:	if(!showEasyFilter) absorbModelRFilter()
	onVisibleChanged:			if(!visible) filterWindow.close()

	Connections
	{
		target:						filterModel
		function onFilterErrorMsgChanged()	{ if (filterModel.filterErrorMsg.length > 0) open();	}
		function onRFilterChanged()			{ absorbModelRFilter();				}
	}

	function toggle()
	{
		opened = !opened
		absorbModelRFilter()
	}

	function open()
	{
		if(!opened)
			toggle();
	}

	function close()
	{
		if(opened)
			toggle();
	}


	function applyAndSendFilter(newFilter)
	{
		filterModel.applyRFilter(newFilter) //Triggers send in FilterModel
		absorbModelRFilter()
	}

	function resetFilter()
	{
		filterModel.resetRFilter()
		absorbModelRFilter()
	}

	function absorbModelRFilter()
	{
		filterEdit.text = filterModel.rFilter
	}

	signal rCodeChanged(string rScript)

	Rectangle
	{
		anchors.fill:	parent
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
	}

	Item
	{
		id:				minWidthCollector

		property int minWidth: 500 * preferencesModel.uiScale

		anchors
		{
			top:			parent.top
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			rightMargin:	Math.min(0, filterContainer.width - minWidthCollector.minWidth)
		}

		Item
		{
			anchors.fill:		parent
			anchors.margins:	1
			visible:			filterContainer.showEasyFilter


			FilterConstructor
			{
				id:				easyFilterConstructor
				onRCodeChanged: filterContainer.rCodeChanged(rScript)
				clip:			true

				anchors
				{
					bottom:	applyEasyFilter.top
					right:	parent.right
					left:	parent.left
					top:	parent.top
				}


				functionModel: ListModel
				{

					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "abs";			functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("absolute value") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "sd";				functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("standard deviation") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "var";			functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("variance") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "sum";			functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("summation") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "prod";			functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("product of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "zScores";		functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("Standardizes the variable") }

					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "min";			functionParameters: "values";			functionParamTypes: "number";						toolTip: qsTr("returns minimum of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "max";			functionParameters: "values";			functionParamTypes: "number";							toolTip: qsTr("returns maximum of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "mean";			functionParameters: "values";			functionParamTypes: "number";								toolTip: qsTr("mean") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "sign";			functionParameters: "values";			functionParamTypes: "number";									toolTip: qsTr("returns the sign of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "round";			functionParameters: "y,n";				functionParamTypes: "number,number";								toolTip: qsTr("rounds y to n decimals") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "length";			functionParameters: "y";				functionParamTypes: "string:number";									toolTip: qsTr("returns number of elements in y") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "median";			functionParameters: "values";			functionParamTypes: "number";												toolTip: qsTr("median") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "ifelse";			functionParameters: "test,then,else";	functionParamTypes: "boolean,boolean:string:number,boolean:string:number";		toolTip: qsTr("if-else statement") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "hasSubstring";	functionParameters: "string,substring";	functionParamTypes: "string,string";											toolTip: qsTr("returns true if string contains substring at least once") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";				functionName: "is.na";			functionParameters: "y";				functionParamTypes: "string:number:boolean";									toolTip: qsTr("Combine with not-operator to filter out rows with missing values (NA) for a column.") }
				}

				function askIfChanged(closeFunc)
				{
					if(jsonChanged() || !lastCheckPassed)
					{
						easySaveDialog.closeFunc = closeFunc
						easySaveDialog.open()
					}
					else
						closeFunc()
				}

				SaveDiscardCancelDialog
				{
					id:		easySaveDialog

					title:	qsTr("Would you like to save your changes to the Filter?")
					text:	qsTr("Your changes will be lost if you don't save them.")

					property var closeFunc: undefined

					onSave:		if(easyFilterConstructor.checkAndApplyFilter()) closeFunc();
					onDiscard:	closeFunc();
				}
			}

			JaspControls.MenuButton
			{
				id:					helpEasyRectangularButton
				height:				33 * jaspTheme.uiScale
				width:				height
				radius:				height
				iconSource:			jaspTheme.iconPath + "info-button.png"
				onClicked:			helpModel.showOrTogglePage("other/easyfilterconstructor");
				toolTip:			qsTr("Open Documentation")
				anchors
				{
					right:			closeButton.left
					top:			parent.top
				}
			}
			JaspControls.MenuButton
			{
				id:					closeButton
				height:				33 * jaspTheme.uiScale
				width:				height
				radius:				height
				iconSource:			jaspTheme.iconPath + "close-button.png"
				onClicked:			easyFilterConstructor.askIfChanged(function() { filterWindow.toggle() } )
				toolTip:			qsTr("Close filter window")
				anchors
				{
					top:			parent.top
					right:			parent.right
					rightMargin:	jaspTheme.generalAnchorMargin
				}
			}

			JaspControls.RectangularButton
			{
				id:			rRectangularButton
				iconSource: jaspTheme.iconPath + "/R.png"
				onClicked:	easyFilterConstructor.askIfChanged(function() { filterContainer.showEasyFilter = false } )
				width:		height
				toolTip:	qsTr("Switch to the R filter")
				anchors
				{
					left:	parent.left
					bottom:	parent.bottom
				}
			}

			JaspControls.RectangularButton
			{
				id:			showInactiveFilteredButtonEasy
				iconSource: dataSetModel.showInactive ? jaspTheme.iconPath + "/eyeOpen.png" : jaspTheme.iconPath + "/eyeClosed.png"
				onClicked:	dataSetModel.showInactive = !dataSetModel.showInactive
				width:		height
				toolTip:	(dataSetModel.showInactive ? qsTr("Hide rows that were filtered out.") : qsTr("Show rows that were filtered out."))
				anchors
				{
					left:	rRectangularButton.right
					bottom:	parent.bottom
					top:	rRectangularButton.top
				}
			}

			JaspControls.RectangularButton
			{
				property bool showApplyNotApplied: easyFilterConstructor.somethingChanged || easyFilterConstructor.showStartupMsg

				id:				applyEasyFilter
				text:			showApplyNotApplied ? qsTr("Apply pass-through filter") : qsTr("Filter applied")
				enabled:		easyFilterConstructor.somethingChanged
				onClicked:		easyFilterConstructor.checkAndApplyFilter()
				toolTip:		showApplyNotApplied ? qsTr("Click to apply filter") : qsTr("Filter is already applied")
				anchors
				{
					left:	showInactiveFilteredButtonEasy.right
					right:	parent.right
					bottom: parent.bottom
					top:	rRectangularButton.top
				}

			}
		}

		Item
		{
							id:						rFilterFields
							visible:				!filterContainer.showEasyFilter
							anchors.fill:			parent
							anchors.margins:		1
			property real	desiredMinimumHeight:	filterButtons.height + (filterErrorScroll.visible ? filterErrorScroll.height : 0 ) + filterEditRectangle.desiredMinimumHeight

			Rectangle
			{
								id:						filterEditRectangle
								color:					jaspTheme.white
								border.width:			1
								border.color:			"lightGrey"
				property real	desiredMinimumHeight:	applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight

				anchors
				{
					top:	parent.top
					bottom:	filterErrorScroll.top
					left:	parent.left
					right:	parent.right
				}

				Image
				{
					id:							backgroundImage

					source:						jaspTheme.iconPath + "/filterConstructorBackground.png"
					anchors.centerIn:			parent

					property real widthScale:	parent.width  / implicitWidth
					property real heightScale:	parent.height / implicitHeight
					property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

					width:						implicitWidth * ratio
					height:						implicitHeight * ratio
				}

				ScrollView
				{
					id:				filterScroller
					anchors.fill:	parent
					clip:			true
					contentWidth:	width
					contentHeight:	filterGeneratedBox.height + filterEditBox.height

					Rectangle
					{
						id:				filterGeneratedBox
						height:			filterGeneratedEdit.contentHeight
						color:			"transparent"
						border.color:	"lightGray"
						border.width:	1

						anchors
						{
							top:	parent.top
							left:	parent.left
							right:	parent.right
						}


						TextArea
						{
							JaspControls.RSyntaxHighlighterQuick
							{
								textDocument:		filterGeneratedEdit.textDocument
							}
							
							id:						filterGeneratedEdit
							anchors.top:			filterGeneratedBox.top
							anchors.left:			resetAllGeneratedFilters.right
							anchors.right:			filterGeneratedBox.right
							text:					filterModel.generatedFilter +"\n"
							height:					contentHeight
							readOnly:				true
							color:					jaspTheme.grayDarker
							selectByMouse:			true
							onActiveFocusChanged:	if(!activeFocus) deselect()
							font.pixelSize:         baseFontSize * preferencesModel.uiScale
							font.family:			jaspTheme.fontCode.family
							wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere

						}

						JaspControls.RectangularButton
						{
							id:						resetAllGeneratedFilters
							width:					dataSetModel.columnsFilteredCount > 0 ? height : 0
							height:					filterGeneratedBox.height
							iconSource:				jaspTheme.iconPath + "eraser_all.png"
							visible:				dataSetModel.columnsFilteredCount > 0
							toolTip:				qsTr("Reset all checkmarks on all labels")
							onClicked:				dataSetModel.resetAllFilters()

							anchors.left:			parent.left
							anchors.verticalCenter:	parent.verticalCenter
							anchors.margins:		1

						}
					}

					Item
					{
						//Must be here because otherwise filterEdit turns its clipping on, because it is in a scrollview...
						id:				filterEditBox
						height:			filterEdit.height
						anchors.top:	filterGeneratedBox.bottom
						anchors.left:	parent.left
						anchors.right:	parent.right

						TextArea
						{

							JaspControls.RSyntaxHighlighterQuick
							{
								textDocument:		filterEdit.textDocument
							}
							
							id:						filterEdit
							height:					contentHeight + 30
							selectByMouse:			true
							onActiveFocusChanged:	if(!activeFocus) deselect()
							font.family:			jaspTheme.fontCode.family
							font.pixelSize:         baseFontSize * preferencesModel.uiScale
							wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere
							color:					jaspTheme.textEnabled

							property bool changedSinceLastApply: text !== filterModel.rFilter

							Keys.onReturnPressed:	(keyEvent) => {
														if(keyEvent.modifiers & Qt.ControlModifier)
														{
															if(filterEdit.changedSinceLastApply)
																filterWindow.applyAndSendFilter(filterEdit.text)
														}
														else
															keyEvent.accepted = false
													}

							anchors
							{
								top:	parent.top
								left:	parent.left
								right:	parent.right
							}
						}
					}
				}

				function askIfChanged(closeFunc)
				{
					if(filterEdit.changedSinceLastApply)
					{
						saveDialog.closeFunc = closeFunc
						saveDialog.open()
					}
					else
						closeFunc()
				}

				SaveDiscardCancelDialog
				{
					id:		saveDialog

					title:	qsTr("Filter Changed")
					text:	qsTr("There are unapplied changes to your filter; what would you like to do?")

					property var closeFunc: undefined

					onSave:		{ filterWindow.applyAndSendFilter(filterEdit.text); closeFunc(); }
					onDiscard:	closeFunc();
				}
			}

			ScrollView
			{
				id:				filterErrorScroll
				height:			filterWindow.minimumHeightTextBoxes

				anchors
				{
					left:	parent.left
					right:	parent.right
					bottom: filterButtons.top
				}

				TextArea
				{
					id:						filterError
					color:					jaspTheme.red
					readOnly:				true
					text:					filterModel.filterErrorMsg + "\n"
					selectByMouse:			true
					onActiveFocusChanged:	if(!activeFocus) deselect()
					font.family:			jaspTheme.fontCode.family
					font.pixelSize:			baseFontSize * preferencesModel.uiScale

					states:
					[
						State
						{
							name: "closed"
							when: filterModel.filterErrorMsg.length === 0
							PropertyChanges { target: filterErrorScroll; visible: false; height: 0 }
						},
						State
						{
							name: "opened"
							when: filterModel.filterErrorMsg.length > 0
							PropertyChanges { target: filterErrorScroll; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)
						}
					]
				}
			}

			Item
			{
				id:				filterButtons
				height:			closeRectangularButton.height
				anchors
				{
					left:		parent.left
					right:		parent.right
					bottom:		parent.bottom
				}

				JaspControls.RectangularButton
				{
					id:				easyRectangularButton
					iconSource:		jaspTheme.iconPath + "/NotR.png"
					onClicked:		filterEditRectangle.askIfChanged(function (){ filterContainer.showEasyFilter = true })
					width:			visible ? height : 0
					toolTip:		qsTr("Switch to the drag and drop filter")
					anchors
					{
						left:		parent.left
						bottom:		parent.bottom
						top:		closeRectangularButton.top
					}
				}

				JaspControls.RectangularButton
				{
					id:			showInactiveFilteredButtonR
					iconSource: dataSetModel.showInactive ? jaspTheme.iconPath + "/eyeOpen.png" : jaspTheme.iconPath + "/eyeClosed.png"
					onClicked:	dataSetModel.showInactive = !dataSetModel.showInactive
					width:		height
					toolTip:	(dataSetModel.showInactive ? qsTr("Hide rows that were filtered out.") : qsTr("Show rows that were filtered out."))
					anchors
					{
						left:	easyRectangularButton.right
						bottom:	parent.bottom
						top:	easyRectangularButton.top
					}
				}

				JaspControls.RectangularButton
				{
					id:			clearRectangularButton
					iconSource: jaspTheme.iconPath + "eraser.png"
					onClicked:	filterWindow.resetFilter()
					width:		visible ? implicitWidth : 0
					height:		filterContainer.buttonsHeight
					visible:	filterEdit.text !== filterModel.defaultRFilter
					toolTip:	qsTr("Reset to default filter")

					anchors
					{
						left:	showInactiveFilteredButtonR.right
						bottom: parent.bottom
						top:	closeRectangularButton.top
					}
				}

				JaspControls.RectangularButton
				{
					id: applyFilter

					property bool filterIsDefault: filterEdit.text === filterModel.defaultRFilter

					text:			qsTr("Apply pass-through filter")
					anchors.left:	clearRectangularButton.right
					anchors.right:	helpButton.left
					anchors.bottom:	parent.bottom
					anchors.top:	closeRectangularButton.top

					onClicked:		{ forceActiveFocus(); filterWindow.applyAndSendFilter(filterEdit.text) }

					toolTip:		qsTr("Click to apply filter")
				}

				JaspControls.RectangularButton
				{
					id:				helpButton
					iconSource:		jaspTheme.iconPath + "info-button.png"
					anchors.right:	closeRectangularButton.left
					anchors.bottom: parent.bottom
					anchors.top:	closeRectangularButton.top

					onClicked:		helpModel.showOrTogglePage("other/rfilterconstructor");
					toolTip:		qsTr("Open Documentation")
				}


				JaspControls.RectangularButton
				{
					id:				closeRectangularButton
					iconSource:		jaspTheme.iconPath + "cross.png"
					anchors.right:	parent.right
					anchors.bottom: parent.bottom

					onClicked:		filterEditRectangle.askIfChanged(function (){ filterWindow.toggle() })
					toolTip:		qsTr("Hide filter")
				}
			}
		}
	}
}
