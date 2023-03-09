package com.embarcadero.firemonkey.advertising;

import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.LoadAdError;

public class AdListenerAdapter extends AdListener {
    private final IAdListener mListener;

    public AdListenerAdapter(IAdListener listener) {
        mListener = listener;
    }

    @Override
    public void onAdClicked() {
        mListener.onAdClicked();
    }

    @Override
    public void onAdClosed() {
        mListener.onAdClosed();
    }

    @Override
    public void onAdFailedToLoad(LoadAdError adError) {
        mListener.onAdFailedToLoad(adError);
    }

    @Override
    public void onAdImpression() {
        mListener.onAdImpression();
    }

    @Override
    public void onAdLoaded() {
        mListener.onAdLoaded();
    }

    @Override
    public void onAdOpened() {
        mListener.onAdOpened();
    }
}
