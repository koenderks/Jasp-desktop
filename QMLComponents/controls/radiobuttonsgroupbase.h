//
// Copyright (C) 2013-2020 University of Amsterdam
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

#ifndef RADIOBUTTONSGROUPBASE_H
#define RADIOBUTTONSGROUPBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include <QMap>

class RadioButtonBase;

class RadioButtonsGroupBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( QString					value			READ value										NOTIFY valueChanged			)
	Q_PROPERTY( RadioButtonBase*		checkedButton	READ checkedButton								NOTIFY valueChanged			)
	Q_PROPERTY( QList<RadioButtonBase*>	buttons			READ buttons									NOTIFY buttonsChanged		)
	Q_PROPERTY( QString					defaultValue	READ defaultValue		WRITE setDefaultValue	NOTIFY defaultValueChanged	)

public:
	RadioButtonsGroupBase(QQuickItem* parent = nullptr);

	bool		isJsonValid(const Json::Value& value)		const	override;
	Json::Value createJson()								const	override;
	void		bindTo(const Json::Value& value)					override;
	void		setUp()												override;

	void registerRadioButton(RadioButtonBase* button);
	void unregisterRadioButton(RadioButtonBase* button);
	void radioButtonValueChanged(RadioButtonBase* button);

	void clickHandler(RadioButtonBase* button);
    
	const QString	value()			const;

	QString	defaultValue()	const	{ return _defaultValue; }
	void	setDefaultValue(const QString& defaultValue);

signals:
	void valueChanged();
	void clicked();
	void buttonsChanged();
	void defaultValueChanged();

protected:

	RadioButtonBase* checkedButton() { return _selectedButton; }
	QList<RadioButtonBase*> buttons() { return _buttons.values(); }

	void _setCheckedButtonHandler();
	void _setCheckedButton(RadioButtonBase* button);


	QSet<RadioButtonBase*>					_buttons;
	RadioButtonBase*						_selectedButton = nullptr;
	QString									_defaultValue;
};

#endif // RADIOBUTTONSGROUPBASE_H
