package com.embarcadero.firemonkey;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.app.NativeActivity;
import android.content.Context;
import android.content.Intent;
import android.graphics.PixelFormat;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Display;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.JobIntentService;

import com.embarcadero.firemonkey.debugger.DebuggerUtils;
import com.embarcadero.firemonkey.device.DeviceArchitectureChecker;
import com.embarcadero.firemonkey.fullscreen.FullScreenManager;
import com.embarcadero.firemonkey.keyboard.VirtualKeyboard;
import com.embarcadero.firemonkey.medialibrary.FMXMediaLibrary;
import com.embarcadero.firemonkey.medialibrary.FMXMediaLibraryListener;
import com.embarcadero.firemonkey.text.FMXEditText;
import com.embarcadero.firemonkey.keyboard.OnKeyboardStateChangedListener;
import com.embarcadero.rtl.notifications.NotificationInfo;

import java.lang.reflect.Method;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class FMXNativeActivity extends NativeActivity implements FMXMediaLibraryListener {
    private static final String TAG = "FMXNativeActivity";
    public static final String ACTION_FCM_NOTIFICATION = "FCMNotification";

    private KeyEvent mLastEvent;
    @NonNull
    private final List<OnActivityListener> listeners = new ArrayList<OnActivityListener>();
    private FMXMediaLibrary mediaLibrary;
    private FMXEditText editText;
    @Nullable
    private Bundle startupFCM;
    @Nullable
    private Dialog activeDialog;

    /**
     * The root container for activity content.
     */
    private ViewGroup contentView;

    /**
     * The list of registered intents actions. Only intents with registered in this list actions will be passed into
     * Delphi side.
     */
    @NonNull
    private final List<String> registeredIntentActions = new ArrayList<String>(Arrays.asList(
                                                                               NotificationInfo.ACTION_NOTIFICATION,
                                                                               ACTION_FCM_NOTIFICATION));

    /**
     * Full screen manager. It is responsible for switching current window state mode.
     */
    @NonNull
    private final FullScreenManager fullScreenManager = new FullScreenManager(this);

    /**
     * Virtual keyboard controller. It allows to open/close soft keyboard.
     */
    private VirtualKeyboard virtualKeyboard;

    /**
     * Current values of window insets.
     */
    @NonNull
    private final Rect windowInsets = new Rect();

    /**
     * Is windows insets defined. At startup Activity doesn't have defined insets values.
     */
    private boolean isWindowInsetsDefined = false;

    /**
     * The listener for notification about window insets changes.
     */
    @Nullable
    private OnActivityInsetsChangedListener onActivityInsetsChangedListener;

    /**
     * The following line is replaced by outside of Java world.
     * To keep security, Release version classes.DEX doesn't contain any debugger support code.
     * see $(TP)\scripts\hudson\android_dex\compile_dex.bat
     *
     * Clever Java compiler will remove some code blocks from binary.
     * For eclipse user, your classes.dex is ALWAYS debug version.
     */
    private static final boolean CLASSES_DEX_DEBUGGER_SUPPORT = true; // TAG:REPLACE

    @SuppressLint("NewApi")
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        // Check device architecture
        DeviceArchitectureChecker.check();

        // Debugger
        if (CLASSES_DEX_DEBUGGER_SUPPORT) {
            DebuggerUtils debuggerUtils = new DebuggerUtils(this);
            debuggerUtils.tryStartDebugger();
        }

        virtualKeyboard = new VirtualKeyboard(this);

        // Some weak Android device can destroy activity in background for saving operative memory.
        // However, Native FMX application is not adapted for this case, because it has bind
        // native instances to Java code. For example, FMX application creates dialog and passes it
        // into Fragment. However, if Operation System destroys activity, when dialog is shown,
        // then Activity will try to recreate fragment here. But since, Fragment doesn't have binding
        // with native Dialog, it will fail. So we are preventing a restoring Fragment here.
        // Client should take care on restoring state his application.
        Bundle bundleWithoutFragmentRestore = createBundleWithoutFragmentRestore(savedInstanceState);
        super.onCreate(bundleWithoutFragmentRestore);

        initWindow();

        Intent aIntent = getIntent();
        Bundle aExtra = aIntent.getExtras();
        if (aExtra != null) {
            // Get properties if this activity was started by clicking Firebase notification
            startupFCM = aExtra;
        }

        editText = new FMXEditText(this);
        editText.setVisibility(View.GONE);

        contentView = (ViewGroup) findViewById(android.R.id.content);
        contentView.addView(editText);
        contentView.setTag("contentView");

        if (Build.VERSION.SDK_INT >= 21) {
            getWindow().getDecorView().setOnApplyWindowInsetsListener(new View.OnApplyWindowInsetsListener() {
                @Override
                public WindowInsets onApplyWindowInsets(View v, WindowInsets insets) {
                    isWindowInsetsDefined = true;
                    windowInsets.top = insets.getSystemWindowInsetTop();
                    windowInsets.bottom = insets.getSystemWindowInsetBottom();
                    windowInsets.left = insets.getSystemWindowInsetLeft();
                    windowInsets.right = insets.getSystemWindowInsetRight();

                    if (onActivityInsetsChangedListener != null) {
                        onActivityInsetsChangedListener.insetsChanged(windowInsets);
                    }

                    // Since setting a listener disables the method onApplyWindowInsets, but decorView has special logic for theme
                    // supporting, we redirect this invoke to decorView.
                    return v.onApplyWindowInsets(insets);
                }
            });
        }

        mediaLibrary = new FMXMediaLibrary(this);
        mediaLibrary.setListener(this);

        virtualKeyboard.addOnKeyboardStateChangedListener(new OnKeyboardStateChangedListener() {
            @Override
            public void onVirtualKeyboardWillShown() {
            }

            @Override
            public void onVirtualKeyboardFrameChanged(@NonNull Rect newFrame) {
            }

            @Override
            public void onVirtualKeyboardWillHidden() {
                FMXNativeActivity.this.runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        fullScreenManager.callback();
                    }
                });
            }
        });
    }

    /**
     * Removes fragment restoring data from {@code bundle}.
     *
     * @param bundle bundle container
     * @return improved bundle with removed "fragments parcelable"
     */
    @Nullable
    private static Bundle createBundleWithoutFragmentRestore(@Nullable Bundle bundle) {
        if (bundle != null) {
            bundle.remove("android:support:fragments");
        }
        return bundle;
    }

    /**
     * Initializes Activity window parameters.
     */
    protected void initWindow() {
        Window window = getWindow();
        window.setFormat(PixelFormat.UNKNOWN);
        if (Build.VERSION.SDK_INT >= 22) {
            window.setClipToOutline(false);
        }
        window.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
    }

    public VirtualKeyboard getVirtualKeyboard() {
        return virtualKeyboard;
    }

    /**
     * Returns activity window insets. This value can be used for retrieving system status bar height and soft keyboard height.
     */
    @NonNull
    public Rect getWindowInsets() {
        return this.windowInsets;
    }

    /**
     * Specifies whether activity's window defined insets or not.
     */
    public boolean isWindowInsetsDefined() {
        return this.isWindowInsetsDefined;
    }

    public void setOnActivityInsetsChangedListener(@Nullable OnActivityInsetsChangedListener onActivityInsetsChangedListener) {
        this.onActivityInsetsChangedListener = onActivityInsetsChangedListener;
    }

    @Nullable
    public OnActivityInsetsChangedListener getOnActivityInsetsChangedListener() {
        return onActivityInsetsChangedListener;
    }

    @Override
    public void onResume() {
        super.onResume();

        fullScreenManager.initFullScreenFSM();
        getWindow().getDecorView().post(new Runnable() {
            @Override
            public void run() {
                fullScreenManager.callback();
            }
        });
    }

    @Override
    public void onPause() {
        super.onPause();
        fullScreenManager.unInitFullScreenFSM();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        virtualKeyboard.getVirtualKeyboardFrameObserver().stopObservation();
    }

    public ViewGroup getContentView() {
        return contentView;
    }

    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        if (hasFocus) {
            fullScreenManager.callback();
        }
        super.onWindowFocusChanged(hasFocus);
    }

    public KeyEvent getLastEvent() {
        return mLastEvent;
    }

    public void addListener(@NonNull OnActivityListener listener) {
        Objects.requireNonNull(listener);

        listeners.add(listener);
    }
    
    public void removeListener(@NonNull OnActivityListener listener) {
        Objects.requireNonNull(listener);

        listeners.remove(listener);
    }

    @Nullable
    public Bundle getStartupFCM() {
        return startupFCM;
    }

    @Override
    public boolean dispatchKeyEvent(KeyEvent event) {
        mLastEvent = event;
        return super.dispatchKeyEvent(event);
    }

    public FMXMediaLibrary getMediaLibrary() {
        return mediaLibrary;
    }

    @SuppressWarnings("deprecation")
    public void showDialog(int id, Dialog dialog) {
        activeDialog = dialog;
        showDialog(id);
    }

    @Override
    @Deprecated
    protected Dialog onCreateDialog(int id) {
        // We don't need to hold instance of dialog.
        Dialog tempDialog = activeDialog;
        activeDialog = null;
        return tempDialog;
    }

    @SuppressLint("NewApi")
    @Nullable
    public Point getRawDisplaySize() {
        Display display = getWindowManager().getDefaultDisplay();
        if (Build.VERSION.SDK_INT >= 17) {
            DisplayMetrics metrics = new DisplayMetrics();
            display.getRealMetrics(metrics);
            return new Point(metrics.widthPixels, metrics.heightPixels);
        } else {
            try {
                Method getRawHeight = Display.class.getMethod("getRawHeight");
                Method getRawWidth = Display.class.getMethod("getRawWidth");
                return new Point((Integer) getRawWidth.invoke(display), (Integer) getRawHeight.invoke(display));
            } catch (Throwable e) {
                Log.w(TAG, "Cannot extract raw screen size: e=" + e);
            }
        }
        return null;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        // Working with result of action of taking photo from camera or Photo Library
        if (FMXMediaLibrary.isRequestForTakingImage(requestCode)) {
            if (resultCode == RESULT_OK) {
                mediaLibrary.handleTakingPhotoRequest(data, requestCode);
            } else {
                for (OnActivityListener listener : listeners) {
                    listener.onCancelReceiveImage(requestCode);
                }
            }
            // Otherwise call the generic ActivityResult messenger
        } else {
            Log.i(TAG, "Request code not from FMXMediaLibrary, calling generic handler.");
            for (OnActivityListener listener : listeners) {
                listener.onReceiveResult(requestCode, resultCode, data);
            }
        }
    }

    public void registerIntentAction(@NonNull String action) {
        Objects.requireNonNull(action, "action");

        registeredIntentActions.add(action);
    }

    /**
     * Checks, Does intent extras contains Firebase push notification data.
     */
    private boolean isFirebaseData(@Nullable Bundle extras){
        return extras != null && extras.containsKey("google.message_id");
    }

    /**
     * Creates special intent with push notification data for internal processing in Delphi side.
     */
    @NonNull
    private Intent preparePushNotificationIntent(@NonNull Bundle extras) {
        Intent intent = new Intent(ACTION_FCM_NOTIFICATION);
        intent.putExtra("fcm", extras);
        return intent;
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);

        if (listeners.size() == 0) {
            return;
        }

        Intent newIntent;
        Bundle extras = intent.getExtras();
        if (isFirebaseData(extras)) {
            newIntent = preparePushNotificationIntent(extras);
        } else {
            newIntent = intent;
        }

        if (registeredIntentActions.contains(newIntent.getAction())) {
            for (OnActivityListener listener : listeners) {
                listener.onReceiveNotification(newIntent);
            }
        }
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        mediaLibrary.onSaveInstanceState(outState);
    }

    @Override
    protected void onRestoreInstanceState(@NonNull Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
        mediaLibrary.onRestoreInstanceState(savedInstanceState);
    }

    /**
     * Returns native edit field for text inputting on FMX side.
     */
    @NonNull
    public FMXEditText getEditText() {
        return editText;
    }

    @Override
    public void onRequestPermissionsResult (int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        for (OnActivityListener listener : listeners) {
            listener.onRequestPermissionsResult(requestCode, permissions, grantResults);
        }
    }

    /**
     * Return unique device identification.
     */
    @NonNull
    public String getDeviceID() {
        String deviceID = Settings.Secure.getString(this.getContentResolver(), Settings.Secure.ANDROID_ID);
        try {
            MessageDigest digest = java.security.MessageDigest.getInstance("MD5");
            digest.update(deviceID.getBytes());
            byte[] messageDigest = digest.digest();

            StringBuilder hexString = new StringBuilder(32);
            for (byte b : messageDigest) {
                String h = Integer.toHexString(0xFF & b);
                if (h.length() == 1) {
                    hexString.append("0");
                }
                hexString.append(h);
            }
            return hexString.toString().toUpperCase();
        } catch (NoSuchAlgorithmException e) {
            Log.w(TAG, "Cannot extract device identification. e=" + e);
        }
        return "";
    }

    @Override
    public void onMediaLibraryAccept(int requestCode) {
        for (OnActivityListener listener : listeners) {
            listener.onReceiveImagePath(requestCode, mediaLibrary.getLastPhotoName());
        }
    }

    public void startJobIntentService(String serviceName, int jobId, Intent workIntent) {
        try {
            Class serviceClass = Class.forName(serviceName);
            Class jobIntentServiceClass = serviceClass.asSubclass(JobIntentService.class);
            Method enqueueWork = JobIntentService.class.getMethod("enqueueWork", Context.class, Class.class, int.class, Intent.class);
            if (enqueueWork != null) {
                enqueueWork.invoke(null, this, jobIntentServiceClass, jobId, workIntent);
            }
        } catch (Throwable e) {
            Log.i(TAG, "Exception occurred enqueuing service work: " + e.getClass().getCanonicalName() + " " + e.getMessage());
        }
    }

    /**
     * Returns full screen manager.
     */
    @NonNull
    public FullScreenManager getFullScreenManager() {
        return fullScreenManager;
    }
}