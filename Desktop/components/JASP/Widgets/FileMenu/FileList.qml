import QtQuick
import QtQuick.Controls
import JASP.Controls as JC


ListView
{
	property var	cppModel:		undefined
	property var	breadCrumbs:	null
	property bool	tabbingEscapes:	false

	id:						listView
	maximumFlickVelocity:	jaspTheme.maximumFlickVelocity
	highlightMoveVelocity:	jaspTheme.maximumFlickVelocity
	highlightMoveDuration:	-1
	clip:					true
	boundsBehavior:			Flickable.StopAtBounds

	spacing:				10
	model:					cppModel
	keyNavigationWraps:		true

	Keys.onEscapePressed:	resourceMenu.forceActiveFocus();
	Keys.onTabPressed:		(event) =>
							{
								if (currentItem.datafile && !currentItem.datafile.activeFocus)
										currentItem.datafile.forceActiveFocus();
								else
								{
									if (tabbingEscapes && currentIndex === count - 1)
										event.accepted = false;
									incrementCurrentIndex();
								}
							}
	Keys.onBacktabPressed: 	(event) =>
							{
								if (currentItem.datafile && currentItem.datafile.activeFocus)
									currentItem.datafile.focus = false;
								else if (tabbingEscapes && currentIndex === 0)
									event.accepted = false;
								else
									decrementCurrentIndex();
							}

	function selectLast()
	{
		forceActiveFocus();
		currentIndex = count - 1;
	}

	function selectFirst()
	{
		forceActiveFocus();
		currentIndex = 0;
	}


	Keys.onLeftPressed: (event) =>
	{
		if(breadCrumbs !== null)
		{
			event.accepted = breadCrumbs.count > 1;

			if(event.accepted)
				breadCrumbs.crumbButtonClicked(breadCrumbs.count - 2)
		}
		else
			event.accepted = false;
	}

	Connections
	{
		target:	listView.model
		function onModelReset() { listView.currentIndex = 0; }
	}

	delegate:	ListItem
	{
		width:					listView.width -  (rightscrollbar.width > 0 ? rightscrollbar.width + listView.spacing : 0)
		cppModel:				listView.cppModel
		hasBreadCrumbs:			listView.breadCrumbs !== null
	}

	JC.JASPScrollBar
	{
		id:				rightscrollbar
		flickable:		parent
		bigBar:			true
	}
}
