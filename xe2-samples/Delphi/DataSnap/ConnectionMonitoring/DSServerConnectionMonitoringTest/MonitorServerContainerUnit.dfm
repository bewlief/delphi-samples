object MonitorServerContainer: TMonitorServerContainer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    AutoStart = True
    HideDSAdmin = False
    Left = 96
    Top = 11
  end
  object DSTCPServerTransport1: TDSTCPServerTransport
    Server = DSServer1
    BufferKBSize = 32
    Filters = <>
    KeepAliveEnablement = kaDefault
    Left = 96
    Top = 73
  end
  object DSHTTPService1: TDSHTTPService
    DSContext = 'datasnap/'
    RESTContext = 'rest/'
    CacheContext = 'cache/'
    Server = DSServer1
    DSHostname = 'localhost'
    DSPort = 211
    Filters = <>
    CredentialsPassThrough = False
    SessionTimeout = 1200000
    HttpPort = 8088
    Active = False
    Left = 96
    Top = 135
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    LifeCycle = 'Session'
    Left = 200
    Top = 11
  end
  object DSProxyGenerator1: TDSProxyGenerator
    MetaDataProvider = DSServerMetaDataProvider1
    TargetUnitName = 'JSProxy.js'
    TargetDirectory = 'web/js'
    Writer = 'Java Script REST'
    Left = 320
    Top = 96
  end
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
    Server = DSServer1
    Left = 320
    Top = 40
  end
  object DSHTTPServiceFileDispatcher1: TDSHTTPServiceFileDispatcher
    Service = DSHTTPService1
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/html'
        Extensions = 'html;htm'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpeg;jpg'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\templates\*'
      end>
    RootDirectory = './web'
    Left = 312
    Top = 176
  end
  object DSServerConnectionMonitor1: TDSServerConnectionMonitor
    Server = DSServer1
    OnConnect = DSServerConnectionMonitor1Connect
    OnDisconnect = DSServerConnectionMonitor1Disconnect
    OnHTTPTrace = DSServerConnectionMonitor1HTTPTrace
    Left = 104
    Top = 208
  end
end
