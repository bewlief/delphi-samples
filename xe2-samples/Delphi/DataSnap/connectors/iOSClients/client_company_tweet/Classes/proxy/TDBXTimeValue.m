
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------


#import "TDBXTimeValue.h"


@implementation TDBXTimeValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:TimeType];
	}
	return self;
}
-(void) dealloc{
	[super dealloc];
}
-(void) SetNull {
	ValueNull = YES;
	DBXInternalValue = 0;
}
-(bool) isNull {
	return ValueNull;
}
-(void) SetAsTDBXTime:(long)value{
	DBXInternalValue = value;
}
-(long) GetAsTDBXTime{
	return DBXInternalValue;
}


@end
