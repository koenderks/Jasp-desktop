import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls	as JaspControls
import JASP

import "FilterConstructor"

FocusScope
{
	id:				computedColumnContainer

	property bool	changed:					computedColumnsInterface.computeColumnUsesRCode ? computeColumnEdit.changed : computedColumnConstructor.somethingChanged
	property int	minimumHeightTextBoxes:		50 * preferencesModel.uiScale
	property real	desiredMinimumHeight:		computeColumnButtons.height + computeColumnErrorScroll.height + (computedColumnsInterface.computeColumnUsesRCode ? computeColumnEditRectangle.desiredMinimumHeight : computedColumnConstructor.desiredMinimumHeight)

	Connections
	{
		target: computedColumnsInterface

		function onComputeColumnJsonChanged()
		{
			computedColumnConstructor.initializeFromJSON(computedColumnsInterface.computeColumnUsesRCode ? "{\"formulas\":[]}" : computedColumnsInterface.computeColumnJson);
		}

		function onComputeColumnRCodeChanged()
		{
			computeColumnEdit.text = computedColumnsInterface.computeColumnRCode;
		}
	}

	Rectangle
	{
		id:				computedColumnWindowBackground
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		anchors.fill:	parent
		z:				-1
	}

	function applyComputedColumn()
	{
		if(computedColumnsInterface.computeColumnUsesRCode)
			computedColumnsInterface.sendCode(computeColumnEdit.text)
		else
		{
			computedColumnConstructor.forceActiveFocus();
			computedColumnConstructor.checkAndApplyFilter()
			computedColumnsInterface.sendCode(computedColumnConstructor.rCode, computedColumnConstructor.jsonConstructed)
		}
	}

	function askIfChangedOrClose()
	{
		if(computedColumnContainer.changed)	
			saveDialog.open()
	}

	Item
	{
		id:				minWidthCollector

		property int minWidth: 400 * preferencesModel.uiScale

		anchors
		{
			top:			parent.top
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			rightMargin:	Math.min(0, computedColumnContainer.width - minWidthCollector.minWidth)
		}

		Item
		{
			id: computeColumnCodeArea

			anchors
			{
				top:			parent.top
				bottom:			computeColumnErrorScroll.top
				left:			parent.left
				right:			parent.right
				topMargin:		1
				leftMargin:		anchors.topMargin
				rightMargin:	anchors.topMargin
			}

			Rectangle
			{
				id:		computeColumnEditRectangle
				color: jaspTheme.white

				border.width: 1
				border.color: jaspTheme.grayLighter

				property real desiredMinimumHeight: computedColumnContainer.minimumHeightTextBoxes

				visible: computedColumnsInterface.computeColumnUsesRCode

				anchors.fill: parent

				TextArea
				{
					id: computeColumnEdit
					
					JaspControls.RSyntaxHighlighterQuick
					{
						textDocument:		computeColumnEdit.textDocument
					}
					

					anchors.top:			parent.top
					anchors.left:			parent.left
					anchors.right:			parent.right
					height:					Math.max(contentHeight + 30, parent.height - 10)
					selectByMouse:			true
					onActiveFocusChanged:	if(!activeFocus) deselect()
					placeholderText:		"Enter your R code here"
					font:					jaspTheme.fontRCode
					wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere
					color:					jaspTheme.textEnabled

					property bool changedSinceLastApply:	text !== computedColumnContainer.lastAppliedcomputeColumn
					property bool changed:					text !== computedColumnsInterface.computeColumnRCode
					
					KeyNavigation.tab:		applyComputedColumnButton


					Keys.onReturnPressed:	(keyEvent) => {
												if(keyEvent.modifiers & Qt.ControlModifier)
												{
													if(changedSinceLastApply)
														computedColumnContainer.applyComputedColumn()
												}
												else
													keyEvent.accepted = false
											}
				}


				Image
				{
					id:							backgroundImage

					source:						jaspTheme.iconPath + "/columnConstructorBackground.png"
					anchors.centerIn:			parent

					property real widthScale:	parent.width  / implicitWidth
					property real heightScale:	parent.height / implicitHeight
					property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

					width:						implicitWidth * ratio
					height:						implicitHeight * ratio
				}
			}

			ComputedColumnsConstructor
			{
				id:						computedColumnConstructor
				anchors.fill:			parent
                anchors.leftMargin:     1
				visible:				!computedColumnsInterface.computeColumnUsesRCode
				
				showGeneratedRCode:		false
				KeyNavigation.tab:		applyComputedColumnButton


				functionModel: ListModel
				{

					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "abs";		functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("absolute value") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "sd";			functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("standard deviation") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "var";		functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("variance") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "sum";		functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("summation") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "prod";		functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("product of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "zScores";	functionParameters: "values";	functionParamTypes: "number";	toolTip: qsTr("Standardizes the variable") }


					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "min";	functionParameters: "values";	functionParamTypes: "number";					toolTip: qsTr("returns minimum of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "max";	functionParameters: "values";	functionParamTypes: "number";					toolTip: qsTr("returns maximum of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "mean";	functionParameters: "values";	functionParamTypes: "number";					toolTip: qsTr("mean") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "sign";	functionParameters: "values";	functionParamTypes: "number";					toolTip: qsTr("returns the sign of values") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "round";	functionParameters: "y,n";		functionParamTypes: "number,number";			toolTip: qsTr("rounds y to n decimals") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "length";	functionParameters: "y";		functionParamTypes: "string:number:boolean";	toolTip: qsTr("returns number of elements in y") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "median";	functionParameters: "values";	functionParamTypes: "number";					toolTip: qsTr("median") }

					ListElement	{ type: "separator" }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "log";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("natural logarithm") }
					ListElement	{ type: "function";	friendlyFunctionName:	"log\u2082";			functionName: "log2";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("base 2 logarithm") }
					ListElement	{ type: "function";	friendlyFunctionName:	"log\u2081\u2080";		functionName: "log10";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("base 10 logarithm") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "logb";			functionParameters: "y,base";			functionParamTypes: "number,number";				toolTip: qsTr("logarithm of y in 'base'") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "exp";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("exponential") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "fishZ";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("Fisher's Z-transform (i.e., the inverse hyperbolic tangent) to transform correlations, numbers between -1 and 1 to the real line") }
					ListElement	{ type: "function";	friendlyFunctionName:	"fishZ\u207B\u00B9";	functionName: "invFishZ";		functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("Inverse Fisher's Z-transform (i.e., the hyperbolic tangent) to transform real numbers to numbers between -1 and 1") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "logit";			functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("Logit transform (i.e., the inverse of the standard logit function, or log-odds transform) converts numbers between 0 and 1 to the real line.") }
					ListElement	{ type: "function";	friendlyFunctionName:	"logit\u207B\u00B9";	functionName: "invLogit";		functionParameters: "y";				functionParamTypes: "number";						toolTip: qsTr("Inverse logit transform (i.e., the standard logit function) converts numbers on the real line to numbers between 0 and 1.") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "BoxCox";			functionParameters: "y,lambda,shift";	functionParamTypes: "number,number,number";			toolTip: qsTr("Two-parameter Box-Cox transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like.") }
					ListElement	{ type: "function";	friendlyFunctionName:	"BoxCox\u207B\u00B9";	functionName: "invBoxCox";		functionParameters: "y,lambda,shift";	functionParamTypes: "number,number,number";			toolTip: qsTr("Inverse two-parameter Box-Cox transform.") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "powerTransform";	functionParameters: "y,lambda,shift";	functionParamTypes: "number,number,number";			toolTip: qsTr("Two-parameter power transform (transforms values greater than -shift) to stabilize variance and attempt to make the data more normal distribution-like.") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "YeoJohnson";		functionParameters: "y,lambda";			functionParamTypes: "number,number";				toolTip: qsTr("Yeo-Johnson transform (transforms any real values) to stabilize variance and attempt to make the data more normal distribution-like.") }

					ListElement	{ type: "separator" }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "cut";			functionParameters: "values,numBreaks";		functionParamTypes: "number,number";                                            toolTip: qsTr("break your data up in numBreaks levels") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "replaceNA";		functionParameters: "column,replaceWith";	functionParamTypes: "string:boolean:number,string:boolean:number";              toolTip: qsTr("replace any missing values (NA) in column by the value in replaceWith") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "ifElse";			functionParameters: "test,then,else";		functionParamTypes: "boolean,boolean:string:number,boolean:string:number";      toolTip: qsTr("if-else statement") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "hasSubstring";	functionParameters: "string,substring";		functionParamTypes: "string,string";											toolTip: qsTr("returns true if string contains substring at least once") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "is.na";			functionParameters: "y";					functionParamTypes: "string:number:boolean";									toolTip: qsTr("returns a boolean vector with TRUE for each missing value in y") }

					ListElement	{ type: "separator" }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "normalDist";     functionParameters: "mean,sd";                  functionParamTypes: "number,number";            toolTip: qsTr("generates data from a Gaussian distribution with specified mean and standard deviation sd") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "tDist";          functionParameters: "df,ncp";                   functionParamTypes: "number,number";            toolTip: qsTr("generates data from t distribution with degrees of freedom df and non-centrality parameter ncp") }

					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "chiSqDist";      functionParameters: "df,ncp";                   functionParamTypes: "number,number";            toolTip: qsTr("generates data from a chi-squared distribution with degrees of freedom df and non-centrality parameter ncp") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "fDist";          functionParameters: "df1,df2,ncp";              functionParamTypes: "number,number,number";     toolTip: qsTr("generates data from an F distribution with specified degrees of freedoms df1, df2 and non-centrality parameter ncp") }

					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "binomDist";      functionParameters: "trials,prob";              functionParamTypes: "number,number";            toolTip: qsTr("generates data from a binomial distribution with specified trials and probability prob") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "negBinomDist";	functionParameters: "targetTrial,prob";         functionParamTypes: "number,number";            toolTip: qsTr("generates data from a negative binomial distribution with specified trials and probability prob") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "geomDist";		functionParameters: "prob";                     functionParamTypes: "number";                   toolTip: qsTr("generates data from a geometric distribution with specified probability prob") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "poisDist";		functionParameters: "lambda";                   functionParamTypes: "number";                   toolTip: qsTr("generates data from a Poisson distribution with specified rate lambda") }
					//ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "integerDist";	functionParameters: "categories,replace,prob";  functionParamTypes: "number,bool,number";       toolTip: qsTr("generates data between 1 and the specified number of categories either with replacement or without and a vector of specified probabilities prob") }

					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "betaDist";       functionParameters: "alpha,beta";               functionParamTypes: "number,number";            toolTip: qsTr("generates data from a beta distribution with specified shapes alpha and beta") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "unifDist";       functionParameters: "min,max";                  functionParamTypes: "number,number";            toolTip: qsTr("generates data from a uniform distribution between min and max") }

					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "gammaDist";      functionParameters: "shape,scale";              functionParamTypes: "number,number";            toolTip: qsTr("generates data from a gamma distribution with specified shape and scale") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "expDist";		functionParameters: "rate";                     functionParamTypes: "number";                   toolTip: qsTr("generates data from an exponential distribution with specified rate") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "logNormDist";    functionParameters: "meanLog,sdLog";            functionParamTypes: "number,number";            toolTip: qsTr("generates data from a log-normal distribution with specified logarithmic mean meanLog and standard deviation sdLog") }
					ListElement	{ type: "function";	friendlyFunctionName:	"";						functionName: "weibullDist";    functionParameters: "shape,scale";              functionParamTypes: "number,number";            toolTip: qsTr("generates data from a Weibull distribution with specified shape and scale") }

					//cut?
					//match?
				}
			}

		}

		ScrollView
		{
			id: computeColumnErrorScroll
			height: visible ? computedColumnContainer.minimumHeightTextBoxes : 0

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: computeColumnButtons.top

			visible: computeColumnError.text.length > 0

			TextArea
			{
				id:						computeColumnError
				color:					jaspTheme.red
				readOnly:				true
				text:					computedColumnsInterface.computeColumnError

				selectByMouse:			true
				onActiveFocusChanged:	if(!activeFocus) deselect()

				font:					jaspTheme.fontCode
				height:					text.length === 0 ? 0 : computeColumnError.contentHeight
			}
		}

		Item
		{
			id:				computeColumnButtons
			height:			helpButton.height
			anchors
			{
				left:		parent.left
				right:		parent.right
				bottom:		parent.bottom
				margins:	1
			}

			JaspControls.RectangularButton
			{
				id:				showGeneratedRCode
				visible:		!computedColumnsInterface.computeColumnUsesRCode
				width:			visible ? implicitWidth : 0

				toolTip:		qsTr("Show generated R code")
				iconSource:		jaspTheme.iconPath + "/R.png"

				anchors.left:	parent.left
				anchors.bottom:	parent.bottom
				anchors.top:	helpButton.top

				onClicked:		computedColumnConstructor.showGeneratedRCode = !computedColumnConstructor.showGeneratedRCode
			}
		
			JaspControls.RectangularButton
			{
				id:					applyComputedColumnButton

				text:				qsTr("Compute column") 
				anchors.left:		showGeneratedRCode.right
				anchors.right:		helpButton.left
				centerTextParent:	true
				anchors.bottom:		parent.bottom
				anchors.top:		helpButton.top
				onClicked:			{ forceActiveFocus(); computedColumnContainer.applyComputedColumn() }
				toolTip:			qsTr("Click to compute column")
				
			}

			JaspControls.RectangularButton
			{
				id:				helpButton
				iconSource:		jaspTheme.iconPath + "info-button.png"
				anchors.right:	parent.right
				anchors.bottom: parent.bottom
				onClicked:		helpModel.showOrTogglePage("other/computedcolumns");
				toolTip:		qsTr("Open Documentation")
			}
		}

		SaveDiscardCancelDialog
		{
			id:			saveDialog
			title:		qsTr("Would you like to save your changes to the Computed Column?")
			text:		qsTr("Your changes will be lost if you don't save them.")
			onSave:
			{
				computedColumnContainer.applyComputedColumn()
			}
		}
	}
}
