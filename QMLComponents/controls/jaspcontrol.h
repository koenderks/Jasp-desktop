#ifndef JASPCONTROL_H
#define JASPCONTROL_H

#include <QQuickItem>
#include <QPropertyAnimation>

#include "utilities/qutils.h"
#include "columntype.h"

class AnalysisForm;
class JASPListControl;
class BoundControl;

///
/// Basic class for all our qml controls
/// Contains all the properties that *must* be there for the QML components defined under Desktop/components/Controls and their bases in Desktop/widgets to function
/// Also defined here are qml-enums for use in qml and of course a whole lot of functionality.
class JASPControl : public QQuickItem
{
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY( ControlType							controlType				READ controlType			WRITE setControlType			NOTIFY controlTypeChanged			)
	Q_PROPERTY( QString								name					READ name					WRITE setName					NOTIFY nameChanged					)
	Q_PROPERTY( QString								title					READ title					WRITE setTitle					NOTIFY titleChanged					) //Basically whatever a human sees on their screen when they look at this specific item.
	Q_PROPERTY( QString								info					READ info					WRITE setInfo					NOTIFY infoChanged					)
	Q_PROPERTY( QString								infoLabel				READ infoLabel				WRITE setInfoLabel				NOTIFY infoLabelChanged				)
	Q_PROPERTY( QString								toolTip					READ toolTip				WRITE setToolTip				NOTIFY toolTipChanged				)
	Q_PROPERTY( QString								helpMD					READ helpMD													NOTIFY helpMDChanged				)
	Q_PROPERTY( bool								isBound					READ isBound				WRITE setIsBound				NOTIFY isBoundChanged				)
	Q_PROPERTY( bool								indent					READ indent					WRITE setIndent					NOTIFY indentChanged				)
	Q_PROPERTY( bool								isDependency			READ isDependency			WRITE setIsDependency			NOTIFY isDependencyChanged			)
	Q_PROPERTY( bool								debug					READ debug					WRITE setDebug					NOTIFY debugChanged					)
	Q_PROPERTY( bool								parentDebug				READ parentDebug											NOTIFY parentDebugChanged			)
	Q_PROPERTY( bool								focusOnTab				READ focusOnTab				WRITE setFocusOnTab				NOTIFY focusOnTabChanged			)
	Q_PROPERTY( bool								hasError				READ hasError				WRITE setHasError				NOTIFY hasErrorChanged				)
	Q_PROPERTY( bool								hasWarning				READ hasWarning				WRITE setHasWarning				NOTIFY hasWarningChanged			)
	Q_PROPERTY( bool								initialized				READ initialized											NOTIFY initializedChanged			)
	Q_PROPERTY( bool								shouldStealHover		READ shouldStealHover		WRITE setShouldStealHover		NOTIFY shouldStealHoverChanged		)
	Q_PROPERTY( QQuickItem						*	childControlsArea		READ childControlsArea		WRITE setChildControlsArea											)
	Q_PROPERTY( JASPControl						*	parentListView			READ parentListViewEx										NOTIFY parentListViewChanged		)
	Q_PROPERTY( QQuickItem						*	innerControl			READ innerControl			WRITE setInnerControl			NOTIFY innerControlChanged			)
	Q_PROPERTY( QQuickItem						*	background				READ background				WRITE setBackground				NOTIFY backgroundChanged			)
	Q_PROPERTY( QQuickItem						*	focusIndicator			READ focusIndicator			WRITE setFocusIndicator			NOTIFY focusIndicatorChanged		)
	Q_PROPERTY( QStringList							dependencyMustContain	READ dependencyMustContain	WRITE setDependencyMustContain	NOTIFY dependencyMustContainChanged	)
	Q_PROPERTY( int									preferredHeight			READ preferredHeight		WRITE setPreferredHeight		NOTIFY preferredHeightChanged		)
	Q_PROPERTY( int									preferredWidth			READ preferredWidth			WRITE setPreferredWidth			NOTIFY preferredWidthChanged		)
	Q_PROPERTY( int									cursorShape				READ cursorShape			WRITE setCursorShape												)
	Q_PROPERTY( bool								hovered					READ hovered												NOTIFY hoveredChanged				)
	Q_PROPERTY( int									alignment				READ alignment				WRITE setAlignment													)
	Q_PROPERTY( Qt::FocusReason						focusReason				READ getFocusReason																				)
	Q_PROPERTY( QVariant							depends					READ explicitDepends		WRITE setExplicitDepends		NOTIFY explicitDependsChanged		)

protected:
	typedef std::set<JASPControl*>			Set;
	typedef std::set<const JASPControl*>	SetConst;

public:
	struct ParentKey
	{
		std::string name, key;
		std::vector<std::string> value;
		ParentKey(const std::string & _name, const std::string & _key, const std::vector<std::string>& _value)
			: name(_name), key(_key), value(_value) {}
	};

	// Any addition here should also be added manually to ControlTypeToFriendlyString... I couldnt get this to work with DECLARE_ENUM...
	enum class ControlType {
		  DefaultControl
		, Expander
		, CheckBox
		, Switch
		, TextField
		, RadioButton
		, RadioButtonGroup
		, VariablesListView
		, ComboBox
		, FactorLevelList
		, InputListView
		, TableView
		, Slider
		, TextArea
		, Button
		, FactorsForm
		, ComponentsList
		, GroupBox
		, TabView
		, VariablesForm
	};

	// Be careful not to reuse a name in a enum type: in QML, they are mixed up with a 'JASP' prefix: JASP.DropNone or JASP.None
	enum class Inclusive		{ None				= 0,															MinMax, MinOnly, MaxOnly };
	enum class DropMode			{ DropNone			= static_cast<int>(Inclusive::MaxOnly)					+ 1,	DropInsert, DropReplace };
	enum class ListViewType		{ AssignedVariables = static_cast<int>(DropMode::DropReplace)				+ 1,	Interaction, AvailableVariables, RepeatedMeasures, Layers, AvailableInteraction };
	enum class CombinationType	{ NoCombination		= static_cast<int>(ListViewType::AvailableInteraction)	+ 1,	CombinationCross, CombinationInteraction, Combination2Way, Combination3Way, Combination4Way, Combination5Way };
	enum class TextType			{ TextTypeDefault	= static_cast<int>(CombinationType::Combination5Way)	+ 1,	TextTypeModel, TextTypeRcode, TextTypeJAGSmodel, TextTypeSource, TextTypeLavaan, TextTypeCSem };
	enum class ModelType		{ Simple			= static_cast<int>(TextType::TextTypeLavaan)			+ 1,	GridInput, CustomContrasts, MultinomialChi2Model, JAGSDataInputModel, FilteredDataEntryModel };
	enum class ItemType			{ String			= static_cast<int>(ModelType::FilteredDataEntryModel)	+ 1,	Integer, Double	};

	Q_ENUM(ControlType)
	Q_ENUM(Inclusive)
	Q_ENUM(DropMode)
	Q_ENUM(ListViewType)
	Q_ENUM(CombinationType)
	Q_ENUM(TextType)
	Q_ENUM(ModelType)
	Q_ENUM(ItemType)

	JASPControl(QQuickItem *parent = nullptr);
	~JASPControl(); //Disconnecting signals right before destroying the object avoids some crashes with qt >= 6.3 on macos m1

	ControlType			controlType()				const	{ return _controlType;				}
	const QString	&	name()						const	{ return _name;						}
	QString				title()						const	{ return _title;					}
	QString				info()						const	{ return _info;						}
	QString				infoLabel()					const	{ return _infoLabel;				}
	virtual bool		infoAddControlType()		const	{ return  false;					}
	virtual bool		infoLabelIsHeader()			const	{ return  false;					}
	virtual bool		infoLabelItalic()			const	{ return  false;					}

	QString				toolTip()					const	{ return _toolTip;					}
	virtual QString		helpMD(int depth = 0)		const;
	virtual bool		hasInfo()					const;
	bool				isBound()					const	{ return _isBound;					}
	bool				nameIsOptionValue()			const	{ return _nameIsOptionValue;		}
	bool				indent()					const	{ return _indent;					}
	bool				isDependency()				const	{ return _isDependency;				}
	bool				initialized()				const	{ return _initialized;				}
	bool				initializedWithValue()		const	{ return _initializedWithValue;		}
	bool				shouldStealHover()			const	{ return _shouldStealHover;			}
	bool				debug()						const	{ return _debug;					}
	bool				parentDebug()				const	{ return _parentDebug;				}
	bool				hasError()					const;
	bool				hasWarning()				const;
	bool				childHasError()				const;
	bool				childHasWarning()			const;
	bool				focusOnTab()				const	{ return activeFocusOnTab();		}
	bool				hasUserInteractiveValue()	const	{ return _hasUserInteractiveValue;	}

	AnalysisForm	*	form()						const	{ return _form;						}
	QQuickItem		*	childControlsArea()			const	{ return _childControlsArea;		}
	JASPListControl	*	parentListView()			const	{ return _parentListView;			}
	JASPControl		*	parentListViewEx()			const;
	QString				parentListViewKey()			const	{ return _parentListViewKey;		}
	QQuickItem		*	innerControl()				const	{ return _innerControl;				}
	QQuickItem		*	background()				const	{ return _background;				}
	QQuickItem		*	focusIndicator()			const	{ return _focusIndicator;			}
	QStringList			dependencyMustContain()		const	{ return _dependencyMustContain;	}
	int					preferredHeight()			const	{ return _preferredHeight;			}
	int					preferredWidth()			const	{ return _preferredWidth;			}
	int					cursorShape()				const	{ return _cursorShape;				}
	bool				hovered()					const;
	int					alignment()					const	{ return _alignment;				}
	Qt::FocusReason		getFocusReason()			const	{ return _focusReason;				}
	bool				dependsOnDynamicComponents() const	{ return _dependsOnDynamicComponents; }
	const QVariant&		explicitDepends()			const	{ return _explicitDepends;			}

	QString				humanFriendlyLabel()		const;

	QVector<JASPControl::ParentKey>	getParentKeys();

	static QString					ControlTypeToFriendlyString(ControlType controlType);
	static QList<JASPControl*>		getChildJASPControls(const QQuickItem* item, bool removeUnecessaryGroups = false);

	virtual void					setUp()										{}
	void							setInitialized(const Json::Value& value = Json::nullValue);
	virtual void					cleanUp()									{ disconnect(); }
	virtual BoundControl		*	boundControl();
	virtual bool					encodeValue()						const	{ return false; }

	const Set					&	depends()							const	{ return _depends; }
	bool							addDependency(		JASPControl* item);
	void							removeDependency(	JASPControl* item);
	virtual JASPControl			*	getChildControl(	const QString & key, const QString & name)	{ return nullptr; }
	void							runRScript(			const QString & script, bool whiteListedVersion = true);
	void							runFilter(			const QString & name);
	virtual void					rScriptDoneHandler(	const QString & result);
	virtual void					filterDoneHandler(	const QString & name, const QString & error);

	virtual QString					friendlyName() const;
	void							addExplicitDependency();

public slots:
	void	setControlType(			ControlType			controlType)		{ _controlType = controlType; }
	void	setChildControlsArea(	QQuickItem		*	childControlsArea);
	void	setFocusOnTab(			bool focus);
	void	setHasError(			bool hasError);
	void	setHasWarning(			bool hasWarning);
	void	setDebug(				bool debug);
	void	setParentDebug(			bool parentDebug);
	void	setFocusIndicator(		QQuickItem* focusIndicator);
	void	setInnerControl(		QQuickItem* innerControl);
	void	setPreferredHeight(		int preferredHeight, bool isBinding = false);
	void	setPreferredWidth(		int preferredWidth, bool isBinding = false);
	void	setAlignment(			int alignment)		{ _alignment = alignment; }

	void	addControlError(			QString message);
	void	addControlErrorTemporary(	QString message);
	void	addControlErrorPermanent(	QString message);
	void	addControlWarning(			QString message);
	void	addControlWarningTemporary(	QString message);
	void	clearControlError();

	void	reconnectWithYourChildren();
	void	parentListViewKeyChanged(const QString& oldName, const QString& newName);
	void	setName(const QString& name);

	GENERIC_SET_FUNCTION(ToolTip				, _toolTip				, toolTipChanged				, QString		)
	GENERIC_SET_FUNCTION(Title					, _title				, titleChanged					, QString		)
	GENERIC_SET_FUNCTION(IsBound				, _isBound				, isBoundChanged				, bool			)
	GENERIC_SET_FUNCTION(Indent					, _indent				, indentChanged					, bool			)
	GENERIC_SET_FUNCTION(IsDependency			, _isDependency			, isDependencyChanged			, bool			)
	GENERIC_SET_FUNCTION(ShouldStealHover		, _shouldStealHover		, shouldStealHoverChanged		, bool			)
	GENERIC_SET_FUNCTION(Background				, _background			, backgroundChanged				, QQuickItem*	)
	GENERIC_SET_FUNCTION(DependencyMustContain	, _dependencyMustContain, dependencyMustContainChanged	, QStringList	)
	GENERIC_SET_FUNCTION(ExplicitDepends		, _explicitDepends		, explicitDependsChanged		, QVariant		)
	GENERIC_SET_FUNCTION(Info					, _info					, infoChanged					, QString		)
	GENERIC_SET_FUNCTION(InfoLabel				, _infoLabel			, infoLabelChanged				, QString		)

private slots:
	void	_hightlightBorder();
	void	_setBackgroundColor();
	void	_setVisible();
	void	_hoveredChangedSlot() { emit hoveredChanged(); }
	void	_resetBindingValue();
	void	_setFocus();
	void	_notifyFormOfActiveFocus();
	void	_checkControlName();

signals:
	void	setOptionBlockSignal(	bool blockSignal);
	void	nameChanged();
	void	isBoundChanged();
	void	indentChanged();
	void	isDependencyChanged();
	void	initializedChanged();
	void	shouldStealHoverChanged();
	void	debugChanged();
	void	parentDebugChanged();
	void	hasErrorChanged();
	void	hasWarningChanged();
	void	focusOnTabChanged();
	void	parentListViewChanged();
	void	innerControlChanged();
	void	backgroundChanged();
	void	focusIndicatorChanged();
	void	infoChanged();
	void	infoLabelChanged();
	void	toolTipChanged();
	void	titleChanged();
	void	helpMDChanged();
	void	dependencyMustContainChanged();
	void	preferredHeightChanged();
	void	preferredWidthChanged();
	void	hoveredChanged();
	void	controlTypeChanged();			// Not used, defined only to suppress warning in QML
	void	boundValueChanged(JASPControl* control);
	void	usedVariablesChanged();
	void	explicitDependsChanged();

	void				requestColumnCreation(std::string columnName, columnType columnType);
	void				requestComputedColumnCreation(std::string columnName);
	void				requestComputedColumnDestruction(std::string columnName);

protected:
	void				componentComplete()									override;
	void				setCursorShape(int shape);
	void				setParentDebugToChildren(bool debug);
	void				focusInEvent(QFocusEvent* event)					override;
	bool				eventFilter(QObject *watched, QEvent *event)		override;
	bool				checkOptionName(const QString& name);
	void				_addExplicitDependency(const QVariant& depends);
	bool				dependingControlsAreInitialized();
	virtual void		_setInitialized(const Json::Value &value);
	bool				printLabelMD(QStringList& md, int depth)			const;

protected:
	Set						_depends;
	ControlType				_controlType;
	AnalysisForm*			_form						= nullptr;
	QString					_name,
							_toolTip,
							_title,
							_parentListViewKey;
	bool					_isBound					= true,
							_indent						= false,
							_initialized				= false,
							_initializedWithValue	= false,
							_debug						= false,
							_parentDebug				= false,
							_hasError					= false,
							_hasWarning					= false,
							_isDependency				= false,
							_useControlMouseArea		= true,
							_shouldStealHover			= false,
							_nameIsOptionValue			= false,
							_hasUserInteractiveValue	= true,
							_hasActiveFocus				= false;
	JASPListControl		*	_parentListView				= nullptr;
	QQuickItem			*	_childControlsArea			= nullptr,
						*	_innerControl				= nullptr,
						*	_background					= nullptr,
						*	_focusIndicator				= nullptr;

	QColor					_defaultBorderColor;
	float					_defaultBorderWidth			= 0;
	QPropertyAnimation		_borderAnimation;
	int						_preferredHeight			= 0,
							_preferredWidth				= 0;
	bool					_preferredHeightBinding		= true,
							_preferredWidthBinding		= true;
	QStringList				_dependencyMustContain;
	QQuickItem			*	_mouseAreaObj				= nullptr;
	int						_cursorShape				= Qt::PointingHandCursor;
	int						_alignment					= Qt::AlignTop | Qt::AlignLeft;
	Qt::FocusReason			_focusReason				= Qt::FocusReason::NoFocusReason;
	bool					_dependsOnDynamicComponents = false;
	QVariant				_explicitDepends;
	QString					_info,
							_infoLabel;


	static QMap<QQmlEngine*, QQmlComponent*>		_mouseAreaComponentMap;
	static QByteArray								_mouseAreaDef;
	static QQmlComponent*							getMouseAreaComponent(QQmlEngine* engine);
	static const QStringList						_optionReservedNames;
};


#endif // JASPCONTROL_H
