object TasksClientModule: TTasksClientModule
  OldCreateOrder = False
  Height = 442
  Width = 575
  object BackendAuth1: TBackendAuth
    Provider = EMSProvider1
    LoginPrompt = False
    UserDetails = <>
    Left = 392
    Top = 264
  end
  object BackendEndpointDeleteNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmDELETE
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{item}'
    Left = 136
    Top = 240
  end
  object BackendEndpointUpdateNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPUT
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{item}'
    Left = 168
    Top = 160
  end
  object BackendEndpointGetNotes: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <>
    Resource = 'Tasks'
    Left = 168
    Top = 88
  end
  object EMSProvider1: TEMSProvider
    AndroidPush.GCMAppID = '000000000000'
    ApiVersion = '1'
    URLHost = 'hostnamegoeshere'
    URLPort = 8080
    Left = 40
    Top = 16
  end
  object BackendEndpointGetNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{item}'
    Left = 136
    Top = 16
  end
  object BackendEndpointAddNote: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPOST
    Params = <>
    Resource = 'Tasks'
    Left = 152
    Top = 304
  end
  object BackendUsers1: TBackendUsers
    Provider = EMSProvider1
    Auth = BackendAuth1
    Left = 376
    Top = 152
  end
  object BackendEndpointGetTaskMessages: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{item}/messages'
    Left = 392
    Top = 32
  end
  object BackendEndpointAddTaskMessage: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPOST
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'item'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{item}/messages'
    Left = 368
    Top = 328
  end
  object PushEvents1: TPushEvents
    Auth = BackendAuth1
    Provider = EMSProvider1
    AutoActivate = False
    AutoRegisterDevice = False
    OnDeviceTokenReceived = PushEvents1DeviceTokenReceived
    OnPushReceived = PushEvents1PushReceived
    Left = 440
    Top = 208
  end
  object BackendEndpointUpdateTaskMessage: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPUT
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'task'
        Options = [poAutoCreated]
      end
      item
        Kind = pkURLSEGMENT
        name = 'message'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{task}/messages/{message}'
    Left = 328
    Top = 384
  end
  object BackendEndpointDeleteTaskMessage: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmDELETE
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'task'
        Options = [poAutoCreated]
      end
      item
        Kind = pkURLSEGMENT
        name = 'message'
        Options = [poAutoCreated]
      end>
    Resource = 'Tasks'
    ResourceSuffix = '{task}/messages/{message}'
    Left = 96
    Top = 376
  end
end
