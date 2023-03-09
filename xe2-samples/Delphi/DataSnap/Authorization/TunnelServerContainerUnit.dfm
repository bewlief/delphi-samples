object ServerContainer5: TServerContainer5
  OldCreateOrder = False
  Height = 271
  Width = 415
  object DSHTTPService1: TDSHTTPService
    DSContext = 'datasnap/'
    RESTContext = 'rest/'
    CacheContext = 'cache/'
    DSHostname = 'localhost'
    DSPort = 211
    Filters = <>
    AuthenticationManager = DSAuthenticationManager1
    CredentialsPassThrough = False
    SessionTimeout = 1200000
    HttpPort = 8082
    Active = False
    Left = 64
    Top = 111
  end
  object DSAuthenticationManager1: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManager1UserAuthenticate
    OnUserAuthorize = DSAuthenticationManager1UserAuthorize
    Roles = <>
    Left = 200
    Top = 112
  end
end
