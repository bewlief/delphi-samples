package com.embarcadero.firemonkey.advertising;

import com.google.android.gms.ads.LoadAdError;

public interface IAdListener {
    void onAdClicked();
    void onAdClosed();
    void onAdFailedToLoad(LoadAdError adError);
    void onAdImpression();
    void onAdLoaded();
    void onAdOpened();
}
