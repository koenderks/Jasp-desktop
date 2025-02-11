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
import QtQuick.Layouts
import JASP.Controls

///Simple help button that shows you the 'helpPage'. Only works when run from a Form.
MenuButton
{
	property string helpPage: ""

	//width: 				20 * preferencesModel.uiScale
	iconSource: 		jaspTheme.iconPath + "info-button.png"
	radius: 			100 * preferencesModel.uiScale
	buttonPadding:		2 * preferencesModel.uiScale
	_scaledDim:			22 * preferencesModel.uiScale
	Layout.alignment: 	Qt.AlignRight
	onClicked: 			helpModel.showOrToggleParticularPageForAnalysis(jaspAnalysis, helpPage)
}
