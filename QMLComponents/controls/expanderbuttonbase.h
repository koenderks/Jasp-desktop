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

#ifndef EXPANDERBUTTONBASE_H
#define EXPANDERBUTTONBASE_H

#include "jaspcontrol.h"

#include <QObject>

class ExpanderButtonBase : public JASPControl
{
	Q_OBJECT
	QML_ELEMENT

public:
	explicit ExpanderButtonBase(QQuickItem *parent = nullptr);

	void	setUp()							override;
	QString helpMD(int depth)		const	override;

	bool infoLabelIsHeader()		const	override	{ return true; }
};

#endif // EXPANDERBUTTONBASE_H
