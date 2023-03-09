/**
 * {*******************************************************}
 * {                                                       }
 * {            Delphi FireMonkey Pickers Service          }
 * {                                                       }
 * {          Implementation of Pickers for Android        }
 * {                                                       }
 * { Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
 * {                                                       }
 * {*******************************************************}
 */
package com.embarcadero.firemonkey.pickers.defaults;

import android.annotation.TargetApi;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.OnItemChangedListener;

import java.util.Objects;

public class DefaultListPicker extends BaseListPicker {

    @NonNull
    private ListPickerFragment pickerFragment;

    @NonNull
    private FMXNativeActivity activity;

    public DefaultListPicker(@NonNull FMXNativeActivity activity) {
        this.pickerFragment = new ListPickerFragment();
        this.activity = Objects.requireNonNull(activity, "activity");
    }

    public void setItemIndex(int itemIndex) {
        pickerFragment.setItemIndex(itemIndex);
    }

    public void setItems(CharSequence[] items) {
        pickerFragment.setItems(items);
    }

    public void setListener(@Nullable OnItemChangedListener listener) {
        pickerFragment.setListener(listener);
    }

    public void hide() {
        pickerFragment.dismiss();
    }

    public boolean isShown() {
        return pickerFragment.isShown();
    }

    @Override
    public void show() {
        if (pickerFragment.getItems().length > 0 && !pickerFragment.isAdded()) {
            pickerFragment.show(activity.getFragmentManager(), "ListPicker");
        }
    }

    @TargetApi(Build.VERSION_CODES.HONEYCOMB)
    @Override
    public void setTheme(int theme) {
        super.setTheme(theme);
        pickerFragment.setTheme(mTheme);
    }
}
