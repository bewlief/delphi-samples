package com.embarcadero.firemonkey.webbrowser;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.Intent;
import android.net.Uri;
import android.webkit.GeolocationPermissions;
import android.webkit.PermissionRequest;
import android.webkit.ValueCallback;
import android.webkit.WebChromeClient;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.OnActivityListener;

import java.util.Objects;

public class NativeWebChromeClient extends WebChromeClient implements OnActivityListener {
    private static final int REQUEST_CODE_CHOOSE_FILES = 4;

    @NonNull
    private FMXNativeActivity activity;
    @Nullable
    private ValueCallback<Uri[]> filePathCallback;

    public NativeWebChromeClient(@NonNull FMXNativeActivity activity) {
        Objects.requireNonNull(activity);

        this.activity = activity;
    }

    @Override
    public void onGeolocationPermissionsShowPrompt(String origin, GeolocationPermissions.Callback callback) {
        callback.invoke(origin, true, true);
    }

    @Override
    public void onPermissionRequest(PermissionRequest request) {
        request.grant(request.getResources());
    }

    @Override
    public boolean onShowFileChooser(WebView webView, ValueCallback<Uri[]> filePathCallback, FileChooserParams fileChooserParams) {
        Intent intent = fileChooserParams.createIntent();

        if (fileChooserParams.getMode() == FileChooserParams.MODE_OPEN_MULTIPLE) {
            intent.putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true);
        }

        String[] mediaTypes = fileChooserParams.getAcceptTypes();

        // Use the 'EXTRA_MIME_TYPES' extra to set multiple media types.
        if (mediaTypes.length > 1) {
            intent.setType("*/*");
            intent.putExtra(Intent.EXTRA_MIME_TYPES, mediaTypes);
        }

        try {
            activity.startActivityForResult(intent, REQUEST_CODE_CHOOSE_FILES);
            activity.addListener(this);

            this.filePathCallback = filePathCallback;
        } catch (ActivityNotFoundException e) {
            filePathCallback.onReceiveValue(null);
        }

        return true;
    }

    @Override
    public void onReceiveImagePath(int requestCode, String fileName) {
    }

    @Override
    public void onCancelReceiveImage(int requestCode) {
    }

    @Override
    public void onReceiveNotification(Intent intent) {
    }

    @Override
    public void onReceiveResult(int requestCode, int resultCode, Intent intent) {
        activity.removeListener(this);

        if (requestCode == REQUEST_CODE_CHOOSE_FILES && filePathCallback != null) {
            Uri[] inputFileUris = null;

            if (resultCode == Activity.RESULT_OK && intent != null) {
                ClipData clipData = intent.getClipData();

                if (clipData == null) {
                    // Handle a single input file.
                    inputFileUris = WebChromeClient.FileChooserParams.parseResult(resultCode, intent);
                } else {
                    // Handle multiple input files.
                    inputFileUris = new Uri[clipData.getItemCount()];

                    for (int i = 0; i < inputFileUris.length; i++) {
                        inputFileUris[i] = clipData.getItemAt(i).getUri();
                    }
                }
            }

            filePathCallback.onReceiveValue(inputFileUris);
            filePathCallback = null;
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
    }
}
