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

#ifndef RADIOBUTTONBASE_H
#define RADIOBUTTONBASE_H

#include "jaspcontrol.h"

class RadioButtonsGroupBase;

class RadioButtonBase : public JASPControl
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY(JASPControl* group READ group NOTIFY groupChanged) // Cannot have a RadioButtonsGroupBase property: compilation error in the moc stuff.

public:
	RadioButtonBase(QQuickItem* parent = nullptr);

	bool infoLabelItalic()	const	override	{ return true; }

	JASPControl* group();

public slots:
	Q_INVOKABLE void registerWithParent();
	Q_INVOKABLE void unregisterRadioButton();
	Q_INVOKABLE void clickHandler();

signals:
	void clicked();
	void groupChanged();

protected slots:
	void valueChangeHandler();

private:
	RadioButtonsGroupBase* _group = nullptr;

};

#endif // RADIOBUTTONBASE_H
