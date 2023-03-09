Tasks resource EMS sample

Projects
========

TasksClientProject - Android, IOS and Windows client.

TasksResourcePackage - EMS custom resource.  This project is configured with EMSDevServer.exe as the host application.

Starting EMSDevServer
=====================
1. Open the TasksResourcePackage project in RAD Studio
2. Run
3. The EMSDevServer should start
4. This line should appear in the EMSDevServer logging memo:
{"RegResource":{"Resource":"Tasks","Endpoints":["GetTask","GetTaskItem","PostTask","PutTaskItem","DeleteTaskItem"...

Using this client
======================


1. Settings
1.1 Connection
1.1.1 Enter host name or IP address where EMSDevServer is running
1.1.2 Click "Test Connection"
1.2 User
1.2.1 Enter a new user name
1.2.2 Enter a new password 
1.2.3 Click "Signup"
2 View tasks
2.1 Click "Tasks" in upper right corner of Settings view
3 Add a task
3.1 Click "+" in the upper right corner of Tasks view
3.2 Enter a title
3.3 Assign to a user
3.4 Click "Save" in upper right corner
4 View task details
4.1 Click on a task in the Tasks view
4.2 Click on forward and back buttons in upper right corner to view other task details
5. Edit a task
5.1 Click on "Edit" in lower right corner of Task Details view
5.2 Modify the task title, status or memo
5.3 Click "Save" in upper right corner
6. View task messages
6.1 Click "Message (1)" button in lower middle of task details view
7. Add a comment
7.1 Click "+" in upper right corner of "Messages" view
7.2 Enter text
7.3 Click "Save" in upper right corner
8. Push notifications
8.1 IOS and Android clients receive push notifications when a task is added or updated, or when a comment is added.
8.2 When a push notification is sent while the app is running, "Notifications" appears at the bottom of the form.
9. View push notifications
9.1 When "Notifications" is shown, click the button to the right of "Notifications".  A list of notifications is shown.
10. Clicking a notification
10.1 Click on a notification takes you to the task or comment
11. Clearing notifications
11.1 Click "Clear" above the list of notifications to clear the list and hide "Notifications".  "Notifications" will appear when another push notification is received. 

Storage
=======

The custom resource stores data in tasks.ini.  Delete tasks.ini to clear all tasks and messages.

emsserver.ini
=============

emsserver.ini must be configured to communicate with the Google and Apple messaging services, for example:

[Server.Push.GCM]
;# This section is for Google Cloud Messaging (GCM) settings.
;# These settings are needed to send push notificatons to an Android device
ApiKey=AIzaSyAulK0JFAKEKEYQF2B07y8lRnOJtFQYTEU


[Server.Push.APNS]
;# This section is for Apple Push Notification Service (APNS).
;# These settings are needed to send push notificatons to an IOS device
CertificateFileName=c:\pushcerts\emspush1.p12
;# Name of .p12 or .pem file
CertificateFilePassword=
;# Password of certificate file.  Leave blank if file does not have a password.

IOS profile and certificate
===========================

IOS clients must have a device profile that enables push notification.

The .p12 file referenced by emsserver.ini must be related to the device profile.

IOS Project options
===================
The suffix of the profile name should be used as part of the "CFBundleIdentifier" and "CFBundleName" settings in "Project/Options/version info" (e.g.; com.yourcompany.emspush1).

"Project/Options/Entitlements List/Receive push notifications" must be set to True.

TEMSProvider
============

The Android.GCMAppId property must be set to your Google Application ID.


AndroidManifest.Template
========================

Android clients must build push notification settings, by putting the customized version of this file in the project directory.

OpenSSL
=======

The EMSDevServer requires openssl to communicate the Google and Apple messaging services.  The Windows openssl libraries must be on the environment path of EMSDevServer.exe.