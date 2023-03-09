
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#import "TDBXDateValue.h"


@implementation TDBXDateValue
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:DateType];
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

-(void) SetAsTDBXDate:(long) value{
	ValueNull = NO;
	DBXInternalValue = value;
}

-(long) GetAsTDBXDate {
	return DBXInternalValue;
}


@end
