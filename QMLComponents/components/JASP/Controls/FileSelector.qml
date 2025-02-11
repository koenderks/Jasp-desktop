import JASP
import JASP.Controls
import QtQuick		 as QT

TextField
{
	id							: selector
	label						: save ? qsTr("Save file to:")			: qsTr("Load file from:")
	property string	caption		: save ? qsTr("Select file to save")	: qsTr("Select file to load")
	property bool	save		: true
	property string	filter		: "*"
	property alias	buttonText	: button.text
	property bool	directory	: false
	property bool	multiple	: false

	implicitWidth				: button.x + button.width

	RoundedButton
	{
		id				: button
		text			: qsTr("Browse")
		z				: 5
		anchors
		{
			leftMargin	: jaspTheme.generalAnchorMargin
			left		: control.right
			top			: selector.top
			bottom		: selector.bottom
		}

		onClicked:
		{
			var browsedFile;

			if (selector.directory)
				browsedFile = messages.browseOpenFolderQML(selector.caption, selector.filter)
			else if (selector.save)
				browsedFile = messages.browseSaveFileDocumentsQML(selector.caption, selector.filter)
			else
				browsedFile = messages.browseOpenFileDocumentsQML(selector.caption, selector.filter, multiple)

			selector.value = browsedFile;
			selector.doEditingFinished();
		}

	}

}
