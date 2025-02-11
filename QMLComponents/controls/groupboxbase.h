//
// Copyright (C) 2013-2024 University of Amsterdam
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

#ifndef GROUPBOXBASE_H
#define GROUPBOXBASE_H

#include "jaspcontrol.h"

class GroupBoxBase : public JASPControl
{
	Q_OBJECT
	QML_ELEMENT

public:
	GroupBoxBase(QQuickItem* parent = nullptr);

	bool infoLabelIsHeader()	const	override;

};

#endif // GROUPBOXBASE_H
