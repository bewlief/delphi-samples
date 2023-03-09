DesktopBeaconCast.groupproj has two projects inside.

VCLBeaconCastDesktop allows to associate a beacon device to the application by filling GUID, major and minor fields. Then click 'Start' button to enable App Tethering manager connect with peers.

FMXClientBeaconCast is the mobile client. Pressing 'Connect' button, tethering profile autoconnects to all VCLBeaconCastDesktop running in the LAN and registers and monitorizes their associated beacons. 
When the device is very close to a monitorized beacon (i.e. less than 0.5 mts), FMXClientBeaconCast runs a remote action in VCLBeaconCastDesktop that takes a desktop screenshot and updates a profile resource with this image. Then FMXClientBeaconCast retrieves this image resource and displays it. 