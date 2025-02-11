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

#ifndef TEXTINPUTBASE_H
#define TEXTINPUTBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"

class TextInputBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( bool		hasScriptError		READ hasScriptError			WRITE setHasScriptError		NOTIFY hasScriptErrorChanged		)
	Q_PROPERTY( QVariant	defaultValue		READ defaultValue			WRITE setDefaultValue		NOTIFY defaultValueChanged			)
	Q_PROPERTY( QString		label				READ label					WRITE setLabel				NOTIFY labelChanged					)
	Q_PROPERTY( QString		afterLabel			READ afterLabel				WRITE setAfterLabel			NOTIFY afterLabelChanged			)
	Q_PROPERTY( QVariant	value				READ value					WRITE setValue				NOTIFY valueChanged					)

public:
	enum TextInputType { IntegerInputType = 0, StringInputType, NumberInputType, PercentIntputType, IntegerArrayInputType, DoubleArrayInputType, ComputedColumnType, AddColumnType, CheckColumnFreeOrMineType, FormulaType, FormulaArrayType};

	TextInputBase(QQuickItem* parent = nullptr);

	bool		isJsonValid(const Json::Value& value)		const	override;
	Json::Value createJson()								const	override;
	void		bindTo(const Json::Value& value)					override;
	void		setUp()												override;
	void		rScriptDoneHandler(const QString& result)			override;
	bool		encodeValue()								const	override;

	TextInputType	inputType()										{ return _inputType; }
	QString			friendlyName() const override;
	bool			hasScriptError()						const	{ return _hasScriptError;		}
	QVariant		defaultValue()							const	{ return _defaultValue;			}
	QVariant		value()									const	{ return _value.isNull() ? _defaultValue : _value; } // Sometimes the value is asked before the control is setup, so in this case give the default value

	const QString	&label()								const	{ return _label;				}
	const QString	&afterLabel()							const	{ return _afterLabel;			}
	bool			infoAddControlType()					const	override		{ return true;	}
	
	void			checkIfColumnIsFreeOrMine();

signals:
	void		formulaCheckSucceeded();
	void		hasScriptErrorChanged();
	void		defaultValueChanged();
	void		valueChanged();
	void		labelChanged();
	void		afterLabelChanged();

public slots:
	GENERIC_SET_FUNCTION(HasScriptError,	_hasScriptError,	hasScriptErrorChanged,	bool		)
	GENERIC_SET_FUNCTION(DefaultValue,		_defaultValue,		defaultValueChanged,	QVariant	)
	GENERIC_SET_FUNCTION(Label,				_label,				labelChanged,			QString		)
	GENERIC_SET_FUNCTION(AfterLabel,		_afterLabel,		afterLabelChanged,		QString		)
	void setValue(const QVariant &value);

private slots:
	void		valueChangedSlot();
	void		setDisplayValue();

private:
	Json::Value	_getJsonValue(const QVariant& value) const;
	bool		_formulaResultInBounds(double result);

	QString		_getPercentValue(double val);
	QString		_getIntegerArrayValue(const std::vector<int>& intValues);
	QString		_getDoubleArrayValue(const std::vector<double>& dblValues);

	void		_setBoundValue();

	TextInputType			_inputType;
	QString					_label,
							_afterLabel;

	bool					_parseDefaultValue	= true;
	QVariant				_defaultValue		= "",
							_value;				// value should not be set: we can then make the difference whether the QML sets directly the value or not
	bool					_hasScriptError		= false;
};

#endif // TEXTINPUTBASE_H
