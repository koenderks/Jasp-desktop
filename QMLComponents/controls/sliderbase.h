#ifndef SLIDERBASE_H
#define SLIDERBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"

class SliderBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	QML_ELEMENT

public:
	SliderBase(QQuickItem* parent = nullptr);

	Json::Value createJson()									const	override;
	bool		isJsonValid(const Json::Value& optionValue)		const	override;
	void		bindTo(const Json::Value& value)						override;
	void		setUp()													override;

signals:
	void		moved();

protected slots:
	void		movedSlot();
	void		_movedDelayedSlot();

protected:
	bool		_changing	= false;
};

#endif // SLIDERBASE_H
