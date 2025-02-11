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

import QtQuick
import JASP.Controls

TextField
{
					id:					textField
					defaultValue:		0
	property var	_prevDefaultValue:	0
	property bool	negativeValues:		false
	property int	min:				negativeValues ? -2147483647 : 0 // 2^32 - 1
	property int	max:				2147483647
	property alias	inclusive:			intValidator.inclusive
	property alias	intValidator:		intValidator
    
					inputType:			"integer"
					validator:			JASPDoubleValidator { id: intValidator; bottom: min; top: max; decimals: 0 }
					cursorShape:		Qt.IBeamCursor
					fieldWidth:			jaspTheme.numericFieldWidth

	onDefaultValueChanged:
	{
		if (_prevDefaultValue == value)
			value = defaultValue

		_prevDefaultValue = defaultValue;
	}
}
