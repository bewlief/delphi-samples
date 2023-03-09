package com.embarcadero.firemonkey.webbrowser;

import android.content.Context;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.embarcadero.firemonkey.FMXNativeActivity;

public class WebBrowser extends WebView {
    @NonNull
    private final WebClient webClient;

    public WebBrowser(@NonNull FMXNativeActivity activity) {
        super(activity);

        webClient = new WebClient();

        setWebViewClient(webClient);
        setWebChromeClient(new NativeWebChromeClient(activity));
        setFocusable(true);
    }

    public void setWebViewListener(@Nullable OnWebViewListener listener) {
        webClient.setWebViewListener(listener);
    }
}
