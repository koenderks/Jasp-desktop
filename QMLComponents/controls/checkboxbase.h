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

#ifndef CHECKBOXBASE_H
#define CHECKBOXBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"

class CheckBoxBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT
	
public:
				CheckBoxBase(QQuickItem* parent = nullptr);
	
	bool		isJsonValid(const Json::Value& value)		const	override;
	Json::Value createJson()								const	override;
	void		bindTo(const Json::Value& value)					override;
	void		setUp()												override;

	void		setChecked(bool checked);
	bool		checked()									const;

signals:
	void		clicked();

protected slots:
	void		clickedSlot();

};

#endif // CHECKBOXBASE_H
