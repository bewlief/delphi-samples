{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ApplicationModel.Contacts;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.ApplicationModel.Contacts.IContactField
  IContactField = interface;
  PIContactField = ^IContactField;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterator_1__IContactField = interface;
  PIIterator_1__IContactField = ^IIterator_1__IContactField;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterable_1__IContactField = interface;
  PIIterable_1__IContactField = ^IIterable_1__IContactField;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContactField>
  IVectorView_1__IContactField = interface;
  PIVectorView_1__IContactField = ^IVectorView_1__IContactField;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContactField>
  IVector_1__IContactField = interface;
  PIVector_1__IContactField = ^IVector_1__IContactField;

  // Windows.ApplicationModel.Contacts.IContact
  IContact = interface;
  PIContact = ^IContact;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContact>
  IIterator_1__IContact = interface;
  PIIterator_1__IContact = ^IIterator_1__IContact;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContact>
  IIterable_1__IContact = interface;
  PIIterable_1__IContact = ^IIterable_1__IContact;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>
  IVectorView_1__IContact = interface;
  PIVectorView_1__IContact = ^IVectorView_1__IContact;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IContact = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IContact = ^AsyncOperationCompletedHandler_1__IVectorView_1__IContact;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVectorView_1__IContact = interface;
  PIAsyncOperation_1__IVectorView_1__IContact = ^IAsyncOperation_1__IVectorView_1__IContact;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Contacts.IContact>
  AsyncOperationCompletedHandler_1__IContact = interface;
  PAsyncOperationCompletedHandler_1__IContact = ^AsyncOperationCompletedHandler_1__IContact;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Contacts.IContact>
  IAsyncOperation_1__IContact = interface;
  PIAsyncOperation_1__IContact = ^IAsyncOperation_1__IContact;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterator_1__ContactFieldType = interface;
  PIIterator_1__ContactFieldType = ^IIterator_1__ContactFieldType;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterable_1__ContactFieldType = interface;
  PIIterable_1__ContactFieldType = ^IIterable_1__ContactFieldType;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IVectorView_1__ContactFieldType = interface;
  PIVectorView_1__ContactFieldType = ^IVectorView_1__ContactFieldType;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IVector_1__ContactFieldType = interface;
  PIVector_1__ContactFieldType = ^IVector_1__ContactFieldType;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>
  IVector_1__IContact = interface;
  PIVector_1__IContact = ^IVector_1__IContact;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVector_1__IContact = interface;
  PAsyncOperationCompletedHandler_1__IVector_1__IContact = ^AsyncOperationCompletedHandler_1__IVector_1__IContact;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVector_1__IContact = interface;
  PIAsyncOperation_1__IVector_1__IContact = ^IAsyncOperation_1__IVector_1__IContact;

  // Windows.ApplicationModel.Contacts Enums

  // Windows.ApplicationModel.Contacts.ContactAddressKind
  ContactAddressKind = (
    Home = 0,
    Work = 1,
    Other = 2
  );
  PContactAddressKind = ^ContactAddressKind;

  // Windows.ApplicationModel.Contacts.ContactAnnotationOperations
  ContactAnnotationOperations = (
    None = 0,
    ContactProfile = 1,
    &Message = 2,
    AudioCall = 4,
    VideoCall = 8,
    SocialFeeds = 16,
    Share = 32
  );
  PContactAnnotationOperations = ^ContactAnnotationOperations;

  // Windows.ApplicationModel.Contacts.ContactAnnotationStoreAccessType
  ContactAnnotationStoreAccessType = (
    AppAnnotationsReadWrite = 0,
    AllAnnotationsReadWrite = 1
  );
  PContactAnnotationStoreAccessType = ^ContactAnnotationStoreAccessType;

  // Windows.ApplicationModel.Contacts.ContactBatchStatus
  ContactBatchStatus = (
    Success = 0,
    ServerSearchSyncManagerError = 1,
    ServerSearchUnknownError = 2
  );
  PContactBatchStatus = ^ContactBatchStatus;

  // Windows.ApplicationModel.Contacts.ContactCardHeaderKind
  ContactCardHeaderKind = (
    Default = 0,
    Basic = 1,
    Enterprise = 2
  );
  PContactCardHeaderKind = ^ContactCardHeaderKind;

  // Windows.ApplicationModel.Contacts.ContactCardTabKind
  ContactCardTabKind = (
    Default = 0,
    Email = 1,
    Messaging = 2,
    Phone = 3,
    Video = 4,
    OrganizationalHierarchy = 5
  );
  PContactCardTabKind = ^ContactCardTabKind;

  // Windows.ApplicationModel.Contacts.ContactChangeType
  ContactChangeType = (
    Created = 0,
    Modified = 1,
    Deleted = 2,
    ChangeTrackingLost = 3
  );
  PContactChangeType = ^ContactChangeType;

  // Windows.ApplicationModel.Contacts.ContactDateKind
  ContactDateKind = (
    Birthday = 0,
    Anniversary = 1,
    Other = 2
  );
  PContactDateKind = ^ContactDateKind;

  // Windows.ApplicationModel.Contacts.ContactEmailKind
  ContactEmailKind = (
    Personal = 0,
    Work = 1,
    Other = 2
  );
  PContactEmailKind = ^ContactEmailKind;

  // Windows.ApplicationModel.Contacts.ContactFieldCategory
  ContactFieldCategory = (
    None = 0,
    Home = 1,
    Work = 2,
    Mobile = 3,
    Other = 4
  );
  PContactFieldCategory = ^ContactFieldCategory;

  // Windows.ApplicationModel.Contacts.ContactFieldType
  ContactFieldType = (
    Email = 0,
    PhoneNumber = 1,
    Location = 2,
    InstantMessage = 3,
    Custom = 4,
    ConnectedServiceAccount = 5,
    ImportantDate = 6,
    Address = 7,
    SignificantOther = 8,
    Notes = 9,
    Website = 10,
    JobInfo = 11
  );
  PContactFieldType = ^ContactFieldType;

  // Windows.ApplicationModel.Contacts.ContactListOtherAppReadAccess
  ContactListOtherAppReadAccess = (
    SystemOnly = 0,
    Limited = 1,
    Full = 2,
    None = 3
  );
  PContactListOtherAppReadAccess = ^ContactListOtherAppReadAccess;

  // Windows.ApplicationModel.Contacts.ContactListOtherAppWriteAccess
  ContactListOtherAppWriteAccess = (
    None = 0,
    SystemOnly = 1,
    Limited = 2
  );
  PContactListOtherAppWriteAccess = ^ContactListOtherAppWriteAccess;

  // Windows.ApplicationModel.Contacts.ContactListSyncStatus
  ContactListSyncStatus = (
    Idle = 0,
    Syncing = 1,
    UpToDate = 2,
    AuthenticationError = 3,
    PolicyError = 4,
    UnknownError = 5,
    ManualAccountRemovalRequired = 6
  );
  PContactListSyncStatus = ^ContactListSyncStatus;

  // Windows.ApplicationModel.Contacts.ContactMatchReasonKind
  ContactMatchReasonKind = (
    Name = 0,
    EmailAddress = 1,
    PhoneNumber = 2,
    JobInfo = 3,
    YomiName = 4,
    Other = 5
  );
  PContactMatchReasonKind = ^ContactMatchReasonKind;

  // Windows.ApplicationModel.Contacts.ContactNameOrder
  ContactNameOrder = (
    FirstNameLastName = 0,
    LastNameFirstName = 1
  );
  PContactNameOrder = ^ContactNameOrder;

  // Windows.ApplicationModel.Contacts.ContactPhoneKind
  ContactPhoneKind = (
    Home = 0,
    Mobile = 1,
    Work = 2,
    Other = 3,
    Pager = 4,
    BusinessFax = 5,
    HomeFax = 6,
    Company = 7,
    Assistant = 8,
    Radio = 9
  );
  PContactPhoneKind = ^ContactPhoneKind;

  // Windows.ApplicationModel.Contacts.ContactQueryDesiredFields
  ContactQueryDesiredFields = (
    None = 0,
    PhoneNumber = 1,
    EmailAddress = 2,
    PostalAddress = 4
  );
  PContactQueryDesiredFields = ^ContactQueryDesiredFields;

  // Windows.ApplicationModel.Contacts.ContactQuerySearchFields
  ContactQuerySearchFields = (
    None = 0,
    Name = 1,
    Email = 2,
    Phone = 4,
    All = -1
  );
  PContactQuerySearchFields = ^ContactQuerySearchFields;

  // Windows.ApplicationModel.Contacts.ContactQuerySearchScope
  ContactQuerySearchScope = (
    Local = 0,
    Server = 1
  );
  PContactQuerySearchScope = ^ContactQuerySearchScope;

  // Windows.ApplicationModel.Contacts.ContactRelationship
  ContactRelationship = (
    Other = 0,
    Spouse = 1,
    Partner = 2,
    Sibling = 3,
    Parent = 4,
    Child = 5
  );
  PContactRelationship = ^ContactRelationship;

  // Windows.ApplicationModel.Contacts.ContactSelectionMode
  ContactSelectionMode = (
    Contacts = 0,
    Fields = 1
  );
  PContactSelectionMode = ^ContactSelectionMode;

  // Windows.ApplicationModel.Contacts.ContactStoreAccessType
  ContactStoreAccessType = (
    AppContactsReadWrite = 0,
    AllContactsReadOnly = 1,
    AllContactsReadWrite = 2
  );
  PContactStoreAccessType = ^ContactStoreAccessType;

  // Windows.ApplicationModel.Contacts.PinnedContactSurface
  PinnedContactSurface = (
    StartMenu = 0,
    Taskbar = 1
  );
  PPinnedContactSurface = ^PinnedContactSurface;

  // Windows.ApplicationModel.Contacts.Provider.AddContactResult
  Provider_AddContactResult = (
    Added = 0,
    AlreadyAdded = 1,
    Unavailable = 2
  );
  PProvider_AddContactResult = ^Provider_AddContactResult;

  // Windows.ApplicationModel.Contacts Interfaces

  // UsedAPI Interface
  // Windows.ApplicationModel.Contacts.IContactField
  IContactField = interface(IInspectable)
  ['{B176486A-D293-492C-A058-DB575B3E3C0F}']
    function get_Type: ContactFieldType; safecall;
    function get_Category: ContactFieldCategory; safecall;
    function get_Name: HSTRING; safecall;
    function get_Value: HSTRING; safecall;
    property Category: ContactFieldCategory read get_Category;
    property Name: HSTRING read get_Name;
    property &Type: ContactFieldType read get_Type;
    property Value: HSTRING read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterator_1__IContactField_Base = interface(IInspectable)
  ['{AA226AF5-FAA5-5353-871C-538099B7C836}']
    function get_Current: IContactField; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIContactField): Cardinal; safecall;
    property Current: IContactField read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterator_1__IContactField = interface(IIterator_1__IContactField_Base)
  ['{C501C1B3-171B-5320-8D84-C3A89A3D4034}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterable_1__IContactField_Base = interface(IInspectable)
  ['{3B012111-C82B-541E-A0C1-37713ED83541}']
    function First: IIterator_1__IContactField; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContactField>
  IIterable_1__IContactField = interface(IIterable_1__IContactField_Base)
  ['{E065EDC5-1D23-5BE5-9998-2736CD4D5297}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContactField>
  IVectorView_1__IContactField = interface(IInspectable)
  ['{1A4C5B35-7EF5-5EEA-94C7-FDF1F617AA7E}']
    function GetAt(index: Cardinal): IContactField; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IContactField; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIContactField): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContactField>
  IVector_1__IContactField_Base = interface(IInspectable)
  ['{F9DD472B-4F50-583A-A3AA-B73AF54806BE}']
    function GetAt(index: Cardinal): IContactField; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IContactField; safecall;
    function IndexOf(value: IContactField; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IContactField); safecall;
    procedure InsertAt(index: Cardinal; value: IContactField); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IContactField); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIContactField): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIContactField); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContactField>
  IVector_1__IContactField = interface(IVector_1__IContactField_Base)
  ['{F9DD472B-4F50-583A-A3AA-B73AF54806BE}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Contacts.IContact
  IContact = interface(IInspectable)
  ['{EC0072F3-2118-4049-9EBC-17F0AB692B64}']
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_Thumbnail: IRandomAccessStreamReference; safecall;
    procedure put_Thumbnail(value: IRandomAccessStreamReference); safecall;
    function get_Fields: IVector_1__IContactField; safecall;
    property Fields: IVector_1__IContactField read get_Fields;
    property Name: HSTRING read get_Name write put_Name;
    property Thumbnail: IRandomAccessStreamReference read get_Thumbnail write put_Thumbnail;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContact>
  IIterator_1__IContact_Base = interface(IInspectable)
  ['{A572C173-800A-58BB-AB24-179959DF2813}']
    function get_Current: IContact; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIContact): Cardinal; safecall;
    property Current: IContact read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.IContact>
  IIterator_1__IContact = interface(IIterator_1__IContact_Base)
  ['{A56C847C-3357-562C-B5BA-83ED9FAF98DB}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContact>
  IIterable_1__IContact_Base = interface(IInspectable)
  ['{63319996-7E0F-552E-872B-7B9ADB1F4997}']
    function First: IIterator_1__IContact; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.IContact>
  IIterable_1__IContact = interface(IIterable_1__IContact_Base)
  ['{B7B7CD65-E654-544B-B163-D0B947B68262}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>
  IVectorView_1__IContact = interface(IInspectable)
  ['{3B57401D-69FE-5482-8459-24B364302EDB}']
    function GetAt(index: Cardinal): IContact; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IContact; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIContact): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IContact_Delegate_Base = interface(IUnknown)
  ['{22DA703A-C764-58CB-9185-CCFAC360025A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IContact; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IContact = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IContact_Delegate_Base)
  ['{7D445A31-CCB8-5A98-B171-76D5C7838BC1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVectorView_1__IContact_Base = interface(IInspectable)
  ['{938328E5-D460-50CC-9C94-7026A6B2E5B2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IContact); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IContact; safecall;
    function GetResults: IVectorView_1__IContact; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IContact read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVectorView_1__IContact = interface(IAsyncOperation_1__IVectorView_1__IContact_Base)
  ['{D6CF7875-8C61-5778-92D9-EAD4901B41FB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Contacts.IContact>
  AsyncOperationCompletedHandler_1__IContact_Delegate_Base = interface(IUnknown)
  ['{A1D09BEE-C181-5419-BD14-8223B95F29A1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IContact; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Contacts.IContact>
  AsyncOperationCompletedHandler_1__IContact = interface(AsyncOperationCompletedHandler_1__IContact_Delegate_Base)
  ['{9203141B-81A2-5F05-A0ED-753846F512B3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Contacts.IContact>
  IAsyncOperation_1__IContact_Base = interface(IInspectable)
  ['{857DB963-F62C-53C4-A3A0-F6BF0C8FD3D3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IContact); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IContact; safecall;
    function GetResults: IContact; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IContact read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Contacts.IContact>
  IAsyncOperation_1__IContact = interface(IAsyncOperation_1__IContact_Base)
  ['{E498900E-0861-5F67-89E0-8F44CBB666E4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterator_1__ContactFieldType_Base = interface(IInspectable)
  ['{2F6D856A-50D4-5173-ABEA-DB6C6B8FC530}']
    function get_Current: ContactFieldType; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PContactFieldType): Cardinal; safecall;
    property Current: ContactFieldType read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterator_1__ContactFieldType = interface(IIterator_1__ContactFieldType_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterable_1__ContactFieldType_Base = interface(IInspectable)
  ['{384B8B1B-CE8E-5781-B3DC-0776D684F658}']
    function First: IIterator_1__ContactFieldType; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IIterable_1__ContactFieldType = interface(IIterable_1__ContactFieldType_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IVectorView_1__ContactFieldType = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): ContactFieldType; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ContactFieldType; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PContactFieldType): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IVector_1__ContactFieldType_Base = interface(IInspectable)
  ['{6FDC2115-1649-54A4-8FAA-3049CEFB05A4}']
    function GetAt(index: Cardinal): ContactFieldType; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ContactFieldType; safecall;
    function IndexOf(value: ContactFieldType; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ContactFieldType); safecall;
    procedure InsertAt(index: Cardinal; value: ContactFieldType); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ContactFieldType); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PContactFieldType): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PContactFieldType); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.ContactFieldType>
  IVector_1__ContactFieldType = interface(IVector_1__ContactFieldType_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>
  IVector_1__IContact_Base = interface(IInspectable)
  ['{945779DB-01D2-5839-8A92-7905EC92B28F}']
    function GetAt(index: Cardinal): IContact; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IContact; safecall;
    function IndexOf(value: IContact; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IContact); safecall;
    procedure InsertAt(index: Cardinal; value: IContact); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IContact); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIContact): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIContact); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>
  IVector_1__IContact = interface(IVector_1__IContact_Base)
  ['{52231997-E8D0-57D7-BAB8-04F9A7F74ED9}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVector_1__IContact_Delegate_Base = interface(IUnknown)
  ['{589B0543-EEAE-5CA2-A63B-76010C64FCCB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVector_1__IContact; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  AsyncOperationCompletedHandler_1__IVector_1__IContact = interface(AsyncOperationCompletedHandler_1__IVector_1__IContact_Delegate_Base)
  ['{017BD13C-6A7D-5AAC-B29A-E276912E023A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVector_1__IContact_Base = interface(IInspectable)
  ['{0F0FF4E8-D25D-53A8-BA87-1A6B23E4315C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVector_1__IContact); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVector_1__IContact; safecall;
    function GetResults: IVector_1__IContact; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVector_1__IContact read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.Contacts.IContact>>
  IAsyncOperation_1__IVector_1__IContact = interface(IAsyncOperation_1__IVector_1__IContact_Base)
  ['{09C2BA26-BC77-529A-B956-4D0569B5341E}']
  end;

implementation

end.
