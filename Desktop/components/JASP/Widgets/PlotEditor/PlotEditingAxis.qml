import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Widgets			as	JASPW
import JASP.Theme
import JASP.Controls		as	JASPC
import JASP.PlotEditor
import JASP

/*
	TODO: it might make sense to add a global title/ box around all the options inside one axis.


	Naming scheme (maybe this should just go in some documentation file):

	JASP/ user				|			Internal name				|			ggplot2
	-------------------------------------------------------------------------------------------
	Title of ...			|			axisTitle					|			name
	Title type				|			titleType					|			NA
	Ticks					|			breaks						|			breaks
	Ticks type				|			breaksType					|			NA
	Range					|			limits						|			limits

	There are also labels, but those can only be edited in the table.

 */

Column
{
					id:			axis
					spacing:	jaspTheme.columnGroupSpacing
	property var	axisModel:	null

	JASPC.CheckBox
	{
		id			: showTitle
		label		: qsTr("Show title")
		checked		: axisModel.titleType !== AxisModel.TitleNull
		onClicked	: axisModel.titleType = (checked ? AxisModel.TitleCharacter : AxisModel.TitleNull)

		JASPC.TextField
		{
			id					: axisTitle
			label				: qsTr("Title");
			fieldWidth			: 200
			value				: axisModel.title
			onEditingFinished	: if(axisModel) axisModel.title = displayValue
		}
	}

	JASPC.CheckBox
	{
		id			: showAxis
		label		: qsTr("Show axis")
		visible		: axisModel.continuous
		checked		: axisModel.breaksType !== AxisModel.BreaksNull
		onClicked	: axisModel.breaksType = (checked ? lastBreakType : AxisModel.BreaksNull)
		columns		: 1

		property int lastBreakType: AxisModel.BreaksRange

		JASPC.RadioButtonGroup
		{
			id						: axisBreaksRadioButton
			title					: qsTr("Ticks:")
			radioButtonsOnSameRow	: true

			JASPC.RadioButton
			{
				id			: axisBreaksRange
				value		: "range"
				label		: qsTr("Specify sequence")
				checked		: if(axisModel) axisModel.continuous ? axisModel.breaksType === AxisModel.BreaksRange	: false
			}
			JASPC.RadioButton
			{
				id			: axisBreaksManual
				value		: "manual"
				label		: qsTr("Set manually")
				checked		: if(axisModel) axisModel.continuous ? axisModel.breaksType === AxisModel.BreaksManual	: true
			}

			onClicked:
			{
				axisModel.breaksType = axisBreaksRange.checked ? AxisModel.BreaksRange : AxisModel.BreaksManual
				showAxis.lastBreakType = axisModel.breaksType
			}
		}

		JASPC.Group
		{
			visible				: axisBreaksRange.checked
			Layout.leftMargin	: jaspTheme.indentationLength

			JASPC.DoubleField
			{
				id					: axisBreaksRangeFrom
				label				: qsTr("from")
				value				: axisModel.from
				onEditingFinished	: if(axisModel) axisModel.from = displayValue
				negativeValues		: true
				fieldWidth			: 2 * jaspTheme.numericFieldWidth
				max					: axisBreaksRangeSteps.value > 0 ? axisBreaksRangeTo.value : Infinity
			}
			JASPC.DoubleField
			{
				id					: axisBreaksRangeTo
				label				: qsTr("to")
				value				: axisModel.to
				onEditingFinished	: if(axisModel) axisModel.to = displayValue
				negativeValues		: true
				fieldWidth			: 2 * jaspTheme.numericFieldWidth
				min					: axisBreaksRangeSteps.value > 0 ? axisBreaksRangeFrom.value : -Infinity
			}
			JASPC.DoubleField
			{
				id					: axisBreaksRangeSteps
				label				: qsTr("steps")
				value				: axisModel.steps
				negativeValues		: true
				onEditingFinished	: if(axisModel) axisModel.steps = displayValue
				fieldWidth			: 2 * jaspTheme.numericFieldWidth
			}

		}

		JASPC.TableView
		{
			visible					: axisBreaksManual.checked
			modelType				: JASP.GridInput
			implicitWidth 			: tableWidth < maxWidth  ? tableWidth : maxWidth
			implicitHeight			: tableHeight
			itemTypePerRow			: [JASP.Double, JASP.String]
			itemType				: JASP.String // To set the String Validator
			rowNames				: [qsTr("Position"), qsTr("Label")]
			cornerText				: qsTr("Tick #")
			minColumn				: 2
			addLeftButton.toolTip	: qsTr("Insert tick left")
			addRightButton.toolTip	: qsTr("Insert tick right")
			deleteButton.toolTip	: qsTr("Delete tick")
			source					: axisModel

			function getColHeaderText(headerText, columnIndex) { return columnIndex + 1; }

			property real maxWidth: axis.width - 2 * jaspTheme.columnGroupSpacing;

		}
	}

	JASPC.Section
	{
		title	: qsTr("Advanced")
		columns	: 1

		JASPC.CheckBox
		{
			label		: qsTr("Parse title as R expression")
			checked		: axisModel.titleType === AxisModel.TitleExpression
			enabled		: axisModel.titleType !== AxisModel.TitleNull
			onClicked	: axisModel.titleType = (checked ? AxisModel.TitleExpression : AxisModel.TitleCharacter)
		}

		JASPC.RadioButtonGroup
		{
			id			: axisLimitsRadioButton
			name		: "axisLimits";
			title		: qsTr("Limits:")
			visible		: axisModel.continuous

			JASPC.RadioButton { value: "data";		label: qsTr("Based on data");	checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsData								}
			JASPC.RadioButton { value: "breaks";	label: qsTr("Based on ticks");	checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsBreaks;	enabled: showAxis.checked	}
			JASPC.RadioButton { value: "manual";	label: qsTr("Set manually");	checked: if(axisModel) axisModel.limitsType === AxisModel.LimitsManual;	columns: 1

				JASPC.DoubleField
				{
					id					: axisLimitsLower
					label				: qsTr("Lower limit")
					negativeValues		: true
					max					: axisModel.limitUpper
					value				: axisModel.limitLower
					onEditingFinished	: if(axisModel) axisModel.limitLower = displayValue
					decimals			: 20
					fieldWidth			: 2 * jaspTheme.numericFieldWidth
				}
				JASPC.DoubleField
				{
					id					: axisLimitsUpper
					label				: qsTr("Upper limit")
					negativeValues		: true
					min					: axisModel.limitLower
					value				: axisModel.limitUpper
					onEditingFinished	: if(axisModel) axisModel.limitUpper = displayValue
					decimals			: 20
					fieldWidth			: 2 * jaspTheme.numericFieldWidth
				}
			}
			onValueChanged: axisModel.limitsType = (axisLimitsRadioButton.value === "data" ? AxisModel.LimitsData : axisLimitsRadioButton.value === "breaks" ? AxisModel.LimitsBreaks : AxisModel.LimitsManual)
		}
	}

}
