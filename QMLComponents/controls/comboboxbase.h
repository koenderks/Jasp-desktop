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

#ifndef COMBOBOXBASE_H
#define COMBOBOXBASE_H

#include "jasplistcontrol.h"
#include "boundcontrols/boundcontrolbase.h"
#include "models/listmodel.h"
#include "models/listmodellabelvalueterms.h"
#include <QMap>

class ComboBoxBase : public JASPListControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( int					currentIndex				READ currentIndex				WRITE setCurrentIndex		NOTIFY currentIndexChanged			)
	Q_PROPERTY( QString				currentText					READ currentText				WRITE setCurrentText		NOTIFY currentTextChanged			)
	Q_PROPERTY( QString				currentValue				READ currentValue				WRITE setCurrentValue		NOTIFY currentValueChanged			)
	Q_PROPERTY( QString				startValue					READ startValue					WRITE setStartValue			NOTIFY startValueChanged			)
	Q_PROPERTY( QString				currentColumnType			READ currentColumnType										NOTIFY currentColumnTypeChanged		)
	Q_PROPERTY( QString				currentColumnTypeIcon		READ currentColumnTypeIcon									NOTIFY currentColumnTypeIconChanged	)
	Q_PROPERTY( QString				longestValue				READ longestValue											NOTIFY longestValueChanged			)

public:
	ComboBoxBase(QQuickItem* parent = nullptr);

	void				bindTo(const Json::Value& value)			override;
	bool				isJsonValid(const Json::Value& optionValue) const	override;
	Json::Value			createJson()								const	override;
	void				setUp()												override;
	ListModel*			model()										const	override	{ return _model;				}
	void				setUpModel()										override;
	QString				helpMD(int depth = 0)						const	override;
	bool				hasInfo()									const	override;
	void				setBoundValue(const Json::Value &value, bool emitChanges = true)	override;

	const QString&		currentText()								const				{ return _currentText;			}
	const QString&		currentValue()								const				{ return _currentValue;			}
	const QString&		startValue()								const				{ return _startValue;			}
	const QString&		currentColumnType()							const				{ return _currentColumnType;	}
	const QString&		currentColumnTypeIcon()						const				{ return _currentColumnTypeIcon;}
	int					currentIndex()								const				{ return _currentIndex;			}
	QString				longestValue()								const				{ return _longestValue;			}

	std::vector<std::string> usedVariables()						const	override;
	bool				encodeValue()								const	override	{ return containsVariables();	}


signals:
	void currentTextChanged();
	void currentValueChanged();
	void startValueChanged();
	void currentColumnTypeChanged();
	void currentColumnTypeIconChanged();
	void currentIndexChanged();
	void activated(int index);
	void longestValueChanged();

protected slots:
	void termsChangedHandler()				override;
	void setCurrentIndex(int index);
	void setCurrentValue(QString value);
	void setCurrentText(QString text);
	void activatedSlot(int index);

	GENERIC_SET_FUNCTION(StartValue,	_startValue,	startValueChanged,	QString	)

protected:
	bool _checkLevelsConstraints()			override;


	ListModelLabelValueTerms*	_model					= nullptr;
	QString						_currentText,
								_currentValue,
								_startValue,
								_currentColumnType,
								_currentColumnRealType,
								_currentColumnTypeIcon;
	std::string					_unusedInitialValue;
	int							_currentIndex			= -1;
	QString						_longestValue;

	int	 _getStartIndex()											const;
	void _resetItemWidth();
	void _setCurrentProperties(int index, bool bindValue = true);
	bool _hasOptionInfo()											const;
	std::string _findType(std::string value)						const;

};

#endif // COMBOBOXBASE_H
