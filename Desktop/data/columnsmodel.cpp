#include "columnsmodel.h"
#include "log.h"
#include "utilities/qutils.h"
#include "mainwindow.h"

ColumnsModel * ColumnsModel::_singleton = nullptr;

ColumnsModel::ColumnsModel(DataSetTableModel *tableModel) 
: QTransposeProxyModel(tableModel), _tableModel(tableModel)
{
	assert(!_singleton);
	_singleton = this;
	
	setSourceModel(tableModel);

	connect(_tableModel, &DataSetTableModel::columnTypeChanged,		this, [&](QString col, int) { emit columnTypeChanged(col); });
	connect(_tableModel, &DataSetTableModel::labelChanged,			this, [&](QString col, QString orgLabel, QString newLabel) { emit labelsChanged(col, {std::make_pair(orgLabel, newLabel) }); } );
	connect(_tableModel, &DataSetTableModel::labelsReordered,		this, &ColumnsModel::labelsReordered	);
	connect(_tableModel, &DataSetTableModel::emptyValuesChanged,	this, &ColumnsModel::dataSetChanged		);

	auto * info = new VariableInfo(_singleton);


	connect(this, &ColumnsModel::namesChanged,							info, &VariableInfo::namesChanged		);
	connect(this, &ColumnsModel::columnsChanged,						info, &VariableInfo::columnsChanged		);
	connect(this, &ColumnsModel::columnTypeChanged,						info, &VariableInfo::columnTypeChanged	);
	connect(this, &ColumnsModel::labelsChanged,							info, &VariableInfo::labelsChanged		);
	connect(this, &ColumnsModel::labelsReordered,						info, &VariableInfo::labelsReordered	);
	connect(this, &ColumnsModel::filterChanged,							info, &VariableInfo::filterChanged		);
	connect(this, &ColumnsModel::dataSetChanged,						info, &VariableInfo::dataSetChanged		);
	connect(this, &QTransposeProxyModel::columnsInserted,				info, &VariableInfo::rowCountChanged	);
	connect(this, &QTransposeProxyModel::columnsRemoved,				info, &VariableInfo::rowCountChanged	);
	connect(this, &QTransposeProxyModel::modelReset,					info, &VariableInfo::rowCountChanged	);
	connect(MainWindow::singleton(), &MainWindow::dataAvailableChanged, info, &VariableInfo::dataAvailableChanged );
}

ColumnsModel::~ColumnsModel()
{ 
	if(_singleton == this) 
		_singleton = nullptr;
}

QString ColumnsModel::getColumnIcon(int colType) const
{
	return getColumnIcon(columnType(colType));
}

QString ColumnsModel::getColumnIcon(int colType, bool isTransformed) const
{
	return !isTransformed ? getColumnIcon(colType) : getColumnIconTransform(colType);
}

QString ColumnsModel::getColumnIcon(columnType colType) const
{
	return VariableInfo::getIconFile(colType, VariableInfo::DefaultIconType);
}

QString ColumnsModel::getColumnIconTransform(int colType) const
{
	return getColumnIconTransform(columnType(colType));
}

QString ColumnsModel::getColumnIconTransform(columnType colType) const
{
	return VariableInfo::getIconFile(colType, VariableInfo::TransformedIconType);
}

int ColumnsModel::getColumnType(const QString & columnName) const
{
	int index = ColumnsModel::singleton()->getColumnIndex(fq(columnName));
	
	if(index == -1)
		return int(columnType::unknown);
	
	return ColumnsModel::singleton()->data(ColumnsModel::singleton()->index(index, 0), ColumnTypeRole).toInt();
}

QString ColumnsModel::getColumnTransformedToolTip(const QString &name, int transformedTo) const
{
	return 	getColumnTransformedToolTip(name, columnType(transformedTo));
}

QString ColumnsModel::getColumnTransformedToolTip(const QString &name, columnType chosenType) const
{
	columnType	realType	= columnType(getColumnType(name));
	
	if(ColumnsModel::singleton()->getColumnIndex(fq(name)) == -1 || chosenType == realType)
		return "";
	
	VariableInfo::InfoType		previewType;
	
	switch(chosenType)
	{
	default:					previewType = VariableInfo::PreviewScale;		break;
	case columnType::ordinal:	previewType	= VariableInfo::PreviewOrdinal;		break;
	case columnType::nominal:	previewType	= VariableInfo::PreviewNominal;		break;
	}
	
	return provideInfo(previewType, name).toString();
	
}


QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	if(index.row() < 0 || index.row() >= rowCount()) return QVariant();

	QString				colName		= QTransposeProxyModel::data(index, int(DataSetPackage::specialRoles::name)).toString();
	columnType			colType		= static_cast<columnType>			(QTransposeProxyModel::data(index, int(DataSetPackage::specialRoles::columnType			)).toInt());
	computedColumnType	codeType	= static_cast<computedColumnType>	(QTransposeProxyModel::data(index, int(DataSetPackage::specialRoles::computedColumnType	)).toInt());

	switch(role)
	{
	case NameRole:					return colName;
	case TypeRole:					return "column";
	case ColumnTypeRole:			return int(colType);
	case ComputedColumnTypeRole:	return int(codeType);
	case IconSourceRole:			return VariableInfo::getIconFile(colType, VariableInfo::DefaultIconType);
	case ToolTipRole:
	{
		QString		usedIn	= colType == columnType::scale		? tr("which can be used in numerical comparisons and mathematical operations.")
							: colType == columnType::ordinal	? tr("which can only be used in (in)equivalence, greater and lesser than comparisons. Not in mathematical operations as subtraction etc, to do so: try converting to scalar first.")
																: tr("which can only be used in (in)equivalence comparisons. Not in greater/lesser-than comparisons or mathematical operations, to do so: try converting to ordinal or scalar first.");

		return tr("The '") + colName + tr("'-column ") + usedIn;
	}
	}

	return QVariant();
}

QVariant ColumnsModel::provideInfo(VariableInfo::InfoType info, const QString& colName, int row) const
{
	ColumnsModel* colModel = ColumnsModel::singleton();

	if (!colModel)
		return QVariant();

	try
	{
		int colIndex = colName.isEmpty() ? 0 : colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return QVariant();

		//remember, the model is transposed:
		QModelIndex qColIndex = index(colIndex, 0),
					qValIndex = index(colIndex, row);

		int			colTypeInt	= data(qColIndex, ColumnsModel::ColumnTypeRole).toInt();
		//columnType	colTypeHere	= static_cast<columnType>(colTypeInt);

		switch(info)
		{
		case VariableInfo::VariableType:				return	colTypeInt;
		case VariableInfo::DoubleValues:				return	QTransposeProxyModel::data(qColIndex,						int(DataSetPackage::specialRoles::valuesDblList));
		case VariableInfo::TotalNumericValues:			return	QTransposeProxyModel::data(qColIndex,						int(DataSetPackage::specialRoles::nonFilteredNumericValuesCount));
		case VariableInfo::TotalLevels:					return	QTransposeProxyModel::data(qColIndex,						int(DataSetPackage::specialRoles::nonFilteredLevels)).toStringList().length();
		case VariableInfo::Labels:						return	QTransposeProxyModel::data(qColIndex,						int(DataSetPackage::specialRoles::nonFilteredLevels));
		case VariableInfo::NameRole:					return	data(qColIndex, ColumnsModel::NameRole);
		case VariableInfo::DataSetRowCount:				return  QTransposeProxyModel::columnCount();
		case VariableInfo::DataSetValue:				return	QTransposeProxyModel::data(qValIndex,						int(DataSetPackage::specialRoles::value));
		case VariableInfo::DataSetValues:				return	QTransposeProxyModel::data(qColIndex,						int(DataSetPackage::specialRoles::valuesStrList));
		case VariableInfo::MaxWidth:					return	QTransposeProxyModel::headerData(colIndex, Qt::Vertical,	int(DataSetPackage::specialRoles::maxColString)).toInt();
		case VariableInfo::SignalsBlocked:				return	_tableModel->synchingData();
		case VariableInfo::VariableNames:				return	getColumnNames();
		case VariableInfo::DataAvailable:				return	MainWindow::singleton()->dataAvailable();
		case VariableInfo::PreviewScale:				return	QTransposeProxyModel::headerData(colIndex, Qt::Vertical,	int(DataSetPackage::specialRoles::previewScale));
		case VariableInfo::PreviewOrdinal:				return	QTransposeProxyModel::headerData(colIndex, Qt::Vertical,	int(DataSetPackage::specialRoles::previewOrdinal));
		case VariableInfo::PreviewNominal:				return	QTransposeProxyModel::headerData(colIndex, Qt::Vertical,	int(DataSetPackage::specialRoles::previewNominal));
		case VariableInfo::DataSetPointer:				return	QVariant::fromValue<void*>(DataSetPackage::pkg()->dataSet());
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return QVariant();
}

bool ColumnsModel::absorbInfo(VariableInfo::InfoType info, const QString &colName, int row, QVariant value)
{
	ColumnsModel* colModel = ColumnsModel::singleton();

	if (!colModel)
		return false;

	try
	{
		int colIndex = colModel->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return false;

		//remember, the model is transposed:
		QModelIndex qColIndex = index(colIndex, 0),
					qValIndex = index(colIndex, row);

		int			colTypeInt	= data(qColIndex, ColumnsModel::ColumnTypeRole).toInt();
		//columnType	colTypeHere	= static_cast<columnType>(colTypeInt);

		switch(info)
		{
		default:										return	false;
		case VariableInfo::DataSetValue:				return	QTransposeProxyModel::setData(qValIndex, value, int(DataSetPackage::specialRoles::value));
		case VariableInfo::DataSetValues:				return	QTransposeProxyModel::setData(qColIndex, value,	int(DataSetPackage::specialRoles::valuesStrList));
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return false;
}

QHash<int, QByteArray> ColumnsModel::roleNames() const
{
	//These should be the same as used in ElementView.qml
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"			},
		{ TypeRole,					"type"					},
		{ ColumnTypeRole,			"columnType"			},
		{ ComputedColumnTypeRole,	"computedColumnType"	},
		{ IconSourceRole,			"columnIcolumnTypeUsercon"			},
		{ ToolTipRole,				"toolTip"				}
	};

	return roles;
}

QStringList ColumnsModel::getColumnNames() const
{
	QStringList result;

	int rows = rowCount();
	for (int i = 0; i < rows; i++)
		result.append(data(index(i, 0), NameRole).toString());

	return result;
}

void ColumnsModel::datasetChanged(  QStringList                             changedColumns,
									QStringList                             missingColumns,
									QMap<QString, QString>					changeNameColumns,
									bool                                    rowCountChanged,
									bool                                    hasNewColumns)
{
	   if(! (missingColumns.size() > 0 || hasNewColumns))
	   {
			   if (changeNameColumns.size() > 0)
					   emit namesChanged(changeNameColumns);
			   else if (changedColumns.size() > 0 || rowCountChanged)
			   {
					   if (rowCountChanged)
					   {
							   changedColumns.clear();
							   for (int i = 0; i < rowCount(); i++)
									   changedColumns.push_back(data(index(i, 0), NameRole).toString());
					   }
					   emit columnsChanged(changedColumns);
			   }
	   }
	   
	emit dataSetChanged(); //For VariableInfoProvider and listeners
}

