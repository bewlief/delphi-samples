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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.OnDateTimeChangedListener;

import java.util.Objects;

public class DefaultTimePicker extends BaseDateTimePicker {

    @NonNull
    private TimePickerFragment pickerFragment;

    @NonNull
    private FMXNativeActivity activity;

    public DefaultTimePicker(@NonNull FMXNativeActivity activity) {
        super();
        this.pickerFragment = new TimePickerFragment(mHour, mMinute);
        this.activity = Objects.requireNonNull(activity, "activity");
    }

    public void setListener(@Nullable OnDateTimeChangedListener listener) {
        pickerFragment.setListener(listener);
    }

    public void hide() {
        pickerFragment.dismiss();
    }

    public boolean isShown() {
        return pickerFragment.isVisible();
    }

    @Override
    public void show() {
        if (!pickerFragment.isAdded()) {
            pickerFragment.setTime(mHour, mMinute);
            pickerFragment.show(activity.getFragmentManager(), "TimePicker");
        }
    }

    @Override
    public void setTheme(int theme) {
        super.setTheme(theme);
        pickerFragment.setTheme(mTheme);
    }
}
