object CMServerContainer: TCMServerContainer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 233
  Width = 262
  object CMServer: TDSServer
    AutoStart = True
    HideDSAdmin = False
    Left = 40
    Top = 11
  end
  object CMServerTransport: TDSTCPServerTransport
    PoolSize = 0
    Server = CMServer
    BufferKBSize = 32
    Filters = <>
    AuthenticationManager = CMAuthManager
    KeepAliveEnablement = kaDefault
    Left = 40
    Top = 73
  end
  object CMServerClass: TDSServerClass
    OnGetClass = CMServerClassGetClass
    Server = CMServer
    LifeCycle = 'Session'
    Left = 136
    Top = 11
  end
  object CMAuthManager: TDSAuthenticationManager
    OnUserAuthorize = CMAuthManagerUserAuthorize
    Roles = <>
    Left = 168
    Top = 88
  end
  object CMHTTPService: TDSHTTPService
    DSContext = 'datasnap/'
    RESTContext = 'rest/'
    CacheContext = 'cache/'
    Server = CMServer
    DSHostname = 'localhost'
    DSPort = 0
    Filters = <>
    CredentialsPassThrough = False
    SessionTimeout = 0
    HttpPort = 8089
    Active = False
    Left = 40
    Top = 143
  end
end
