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

public class DefaultDatePicker extends BaseDateTimePicker {

    @NonNull
    private DatePickerFragment pickerFragment;

    @NonNull
    private FMXNativeActivity activity;

    public DefaultDatePicker(@NonNull FMXNativeActivity activity) {
        super();
        this.pickerFragment = new DatePickerFragment(mYear, mMonth, mDay);
        this.activity = Objects.requireNonNull(activity, "activity");
    }

    public void setListener(@Nullable OnDateTimeChangedListener listener) {
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
        if (!pickerFragment.isAdded()) {
            pickerFragment.setDate(mYear, mMonth, mDay);
            pickerFragment.show(activity.getFragmentManager(), "DatePicker");
        }
    }

    @Override
    public void setTheme(int theme) {
        super.setTheme(theme);
        pickerFragment.setTheme(mTheme);
    }
}