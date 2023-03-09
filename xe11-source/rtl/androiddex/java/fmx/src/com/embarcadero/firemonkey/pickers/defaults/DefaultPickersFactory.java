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

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.BaseDateTimePicker;
import com.embarcadero.firemonkey.pickers.BaseListPicker;
import com.embarcadero.firemonkey.pickers.BasePickersFactory;

public class DefaultPickersFactory extends BasePickersFactory {

    @NonNull
    @Override
    public BaseDateTimePicker createDatePicker(@NonNull FMXNativeActivity activity) {
        return new DefaultDatePicker(activity);
    }

    @NonNull
    @Override
    public BaseDateTimePicker createTimePicker(@NonNull FMXNativeActivity activity) {
        return new DefaultTimePicker(activity);
    }

    @NonNull
    @Override
    public BaseListPicker createListPicker(@NonNull FMXNativeActivity activity) {
        return new DefaultListPicker(activity);
    }

}
