
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#import "TDBXUInt8Value.h"


@implementation TDBXUInt8Value
-(id)init{
	self = [super init];
	if (self) {
		[self setDBXType:UInt8Type];
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
-(void) SetAsUInt8:(unsigned int)value{
	ValueNull = NO;
	DBXInternalValue = value;
	
}
-(unsigned int) GetAsUInt8{
	return DBXInternalValue;
}


@end
