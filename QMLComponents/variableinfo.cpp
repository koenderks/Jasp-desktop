#include "variableinfo.h"
#include "jasptheme.h"
#include "QQmlContext"
#include "QTimer"

VariableInfo* VariableInfo::_singleton = nullptr;

VariableInfo::VariableInfo(VariableInfoProvider* providerInfo) :
	QObject(providerInfo->providerModel()), _provider(providerInfo)
{
	if (_singleton == nullptr)
		_singleton = this;
}

VariableInfo *VariableInfo::info()
{
	return _singleton;
}

QString VariableInfo::getIconFile(columnType colType, VariableInfo::IconType type)
{
	QString iconType;
	
	switch(type)
	{
	default:
	case VariableInfo::DefaultIconType:		iconType = "";				break;
	case VariableInfo::DisabledIconType:	iconType = "-disabled";		break;
	case VariableInfo::InactiveIconType:	iconType = "-inactive";		break;		
	case VariableInfo::TransformedIconType:	iconType = "-transformed";	break;
	}
	
	return QString("%1variable-%2%3.svg").arg(JaspTheme::currentIconPath()).arg(columnTypeToQString(colType)).arg(iconType);
}

QString VariableInfo::getTypeFriendly(columnType colType)
{
	switch(colType)
	{
	case columnType::scale:			return tr("Scale");
	case columnType::ordinal:		return tr("Ordinal");
	case columnType::nominal:		return tr("Nominal");
	default:						break;
	}
	
	return tr("Unknown");
}


int VariableInfo::rowCount()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataSetRowCount).toInt() : 0;
}

bool VariableInfo::dataAvailable()
{
	return _provider ? _provider->provideInfo(VariableInfo::DataAvailable).toBool() : false;
}

DataSet *VariableInfo::dataSet()
{
	return _provider ? reinterpret_cast<DataSet*>(_provider->provideInfo(VariableInfo::DataSetPointer).value<void*>()) : nullptr;
}
