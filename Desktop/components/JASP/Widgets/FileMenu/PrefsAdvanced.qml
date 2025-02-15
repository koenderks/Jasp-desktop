import QtQuick
import QtQuick.Controls
import JASP.Widgets
import JASP.Controls

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) rememberModulesSelected.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Advanced Preferences")
			helpfile:		"preferences/PrefsAdvanced"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}


		PrefsGroupRect
		{
			title:				qsTr("Modules options")

			CheckBox
			{
				id:					rememberModulesSelected
				label:				qsTr("Remember enabled modules")
				checked:			preferencesModel.modulesRemember
				onCheckedChanged:	preferencesModel.modulesRemember = checked
				toolTip:			qsTr("Continue where you left of the next time JASP starts.\nEnabling this option makes JASP remember which Modules you've enabled.")
				
				KeyNavigation.tab:		cranRepoUrl
			}

			Item
			{
				id:		cranRepoUrlItem
				width:	parent.width
				height:	cranRepoUrl.height

				Label
				{
					id:		cranRepoUrlLabel
					text:	qsTr("Change the CRAN repository: ")

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						margins:		jaspTheme.generalAnchorMargin
					}
				}

				PrefsTextInput
				{
					id:					cranRepoUrl

					text:				preferencesModel.cranRepoURL
					onEditingFinished:	preferencesModel.cranRepoURL = text
					nextEl:				githubPatDefault

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			cranRepoUrlLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	githubPatDefault
				}
			}


			CheckBox
			{
				id:					githubPatDefault
				label:				qsTr("Use default PAT for Github")
				checked:			preferencesModel.githubPatUseDefault
				onCheckedChanged:	preferencesModel.githubPatUseDefault = checked
				toolTip:			qsTr("Either use the bundled GITHUB_PAT or, if available, use the one set in environment variables.")

				KeyNavigation.tab:		githubPatCustomToken
			}

			Item
			{
				id:			githubPatCustomTokenItem
				width:		parent.width
				height:		cranRepoUrl.height
				enabled:	!preferencesModel.githubPatUseDefault

				Label
				{
					id:					githubPatCustomLabel
					text:				qsTr("Private GITHUB_PAT:")

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}
				}

				PrefsTextInput
				{
					id:					githubPatCustomToken

					text:				preferencesModel.githubPatCustom
					onEditingFinished:	preferencesModel.githubPatCustom = text

					nextEl:				developerMode

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			githubPatCustomLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					textInput.echoMode:	TextInput.Password

					KeyNavigation.tab:		developerMode
				}
			}

			CheckBox
			{
				id:					developerMode
				label:				qsTr("Developer mode")
				checked:			preferencesModel.developerMode
				onCheckedChanged:	preferencesModel.developerMode = checked
				toolTip:			qsTr("To use JASP Modules enable this option.")
				
				KeyNavigation.tab:	generateMarkdown
			}

			CheckBox
			{
				id:					generateMarkdown
				label:				qsTr("Generate markdown files for help")
				toolTip:			qsTr("Enabling this will generate markdown helpfile from the info at qml options.")
				checked:			preferencesModel.generateMarkdown
				onCheckedChanged:	preferencesModel.generateMarkdown = checked
				visible:			preferencesModel.developerMode
				enabled:			preferencesModel.developerMode
				KeyNavigation.tab:	cleanModulesFolder

			}

	
			RoundedButton
			{	
				id:					cleanModulesFolder
				text:				qsTr("Clear installed modules and packages")
				toolTip:			qsTr("This will erase the 'renv' and 'Modules' folders in the appdata.")
				onClicked:			mainWindow.clearModulesFoldersUser();

				KeyNavigation.tab:		directLibpathDevModEnabled
				activeFocusOnTab:		true
			}
		}

		PrefsGroupRect
		{
			id:					editDeveloperFolder
			title:				qsTr("Development module")
			visible:			preferencesModel.developerMode
			enabled:			preferencesModel.developerMode

			CheckBox
			{
				id:					directLibpathDevModEnabled
				label:				qsTr("Enable direct libpath mode")
				checked:			preferencesModel.directLibpathEnabled
				onCheckedChanged:	preferencesModel.directLibpathEnabled = checked
				toolTip:			qsTr("Load modules from a binary in an R-library instead of installing it from sources.")
				visible:			preferencesModel.developerMode

				KeyNavigation.tab:	editDeveloperFolder
			}


			Item
			{
				width:				parent.width
				height:				browseDeveloperFolderButton.height
				enabled:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled


				RectangularButton
				{
					id:						browseDeveloperFolderButton
					text:					qsTr("Source folder:")
					onClicked:				preferencesModel.browseDeveloperFolder()
					anchors.left:			parent.left
					anchors.leftMargin:		jaspTheme.subOptionOffset
					toolTip:				qsTr("Browse to your JASP Module folder.")

					KeyNavigation.tab:		developerFolderText.textInput
					activeFocusOnTab:		true

				}

				PrefsTextInput
				{
					id:					developerFolderText

					text:				preferencesModel.developerFolder
					onEditingFinished:	preferencesModel.developerFolder = text
					nextEl:				directLibPathLabel

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			browseDeveloperFolderButton.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}
				}
			}

			Item
			{
				id:					directLibpath
				enabled:			preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
				width:				parent.width
				height:				cranRepoUrl.height

				RectangularButton
				{
					id:						directLibPathLabel
					text:					qsTr("Libpath:")
					width:					Math.max(directDevModName.implicitWidth, directLibPathLabel.implicitWidth)
					onClicked:				preferencesModel.browseDeveloperLibPathFolder()
					activeFocusOnTab:		true
					KeyNavigation.tab:		directLibpathFolder.textInput
					KeyNavigation.backtab:	directLibpathDevModEnabled

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}


				}

				PrefsTextInput
				{
					id:					directLibpathFolder

					text:				preferencesModel.directLibpathFolder
					onEditingFinished:	preferencesModel.directLibpathFolder = text

					nextEl:				moduleName

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			directLibPathLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	moduleName

					toolTip:			qsTr("Choose the R library where you installed the development module")
				}
			}

			Item {

				id:					directDevMod
				enabled:			preferencesModel.developerMode &&preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
				width:				parent.width
				height:				cranRepoUrl.height

				Label
				{
					id:					directDevModName
					text:				qsTr("Module name:")
					width:				Math.max(directDevModName.implicitWidth, directLibPathLabel.implicitWidth)

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}
				}

				PrefsTextInput
				{
					id:					moduleName

					text:				preferencesModel.directDevModName
					onEditingFinished:	preferencesModel.directDevModName = text

					nextEl:				logToFile

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			directDevModName.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	logToFile
					toolTip:			qsTr("Enter the (package)name of the development module you want to load")
				}
			}
		}
		
		PrefsGroupRect
		{
			id:		loggingGroup
			title:	qsTr("Logging options")

			CheckBox
			{
				id:					logToFile
				label:				qsTr("Log to file")
				checked:			preferencesModel.logToFile
				onCheckedChanged:	preferencesModel.logToFile = checked
				toolTip:			qsTr("To store debug-logs of JASP in a file, check this box.")

				KeyNavigation.tab:		maxLogFilesSpinBox
			}

			Item
			{
				id:					loggingSubGroup
				x:					jaspTheme.subOptionOffset
				height:				maxLogFilesSpinBox.height
				width:				showLogs.x + showLogs.width
				enabled:			preferencesModel.logToFile


				SpinBox
				{
					id:					maxLogFilesSpinBox
					value:				preferencesModel.logFilesMax
					onValueChanged:		if(value !== "") preferencesModel.logFilesMax = value
					from:				5 //Less than 5 makes no sense as on release you get 1 for Desktop and 4 from the Engines
					to:					1000000
					defaultValue:		10
					stepSize:			1

					KeyNavigation.tab:	showLogs
					text:				qsTr("Max logfiles to keep: ")

					anchors
					{
						leftMargin:	jaspTheme.generalAnchorMargin
						left:		parent.left
						top:		showLogs.top
						bottom:		showLogs.bottom
					}
				}

				RoundedButton
				{
					id:			showLogs
					text:		qsTr("Show logs")
					onClicked:	mainWindow.showLogFolder();
					anchors
					{
						margins:	jaspTheme.generalAnchorMargin
						left:		maxLogFilesSpinBox.right
					}

					KeyNavigation.tab:		maxEngineCount
					activeFocusOnTab:		true
				}
			}
		}
		
		PrefsGroupRect
		{
			id:		engineGroup
			title:	qsTr("Engine options")
			
			SpinBox
			{
				id:					maxEngineCount
				value:				preferencesModel.maxEngines
				onValueChanged:		if(value != "") preferencesModel.maxEngines = value
				from:				1
				to:					preferencesModel.maxEnginesAdmin > 0 ? preferencesModel.maxEnginesAdmin : 16
				defaultValue:		Math.max(preferencesModel.maxEnginesAdmin, 4)
				stepSize:			1

				KeyNavigation.tab:	showEnginesWindow
				activeFocusOnTab:			true
				text:				qsTr("Maximum number of engines: ")
			}

			RoundedButton
			{
				id:					showEnginesWindow
				text:				qsTr("Show engines")
				onClicked:			mainWindow.showEnginesWindow()
				activeFocusOnTab:		true
			}
		}
	}
}
