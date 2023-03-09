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
package com.embarcadero.firemonkey.pickers;

import androidx.annotation.NonNull;

import com.embarcadero.firemonkey.FMXNativeActivity;
import com.embarcadero.firemonkey.pickers.defaults.DefaultPickersFactory;

/**
 * Base factory for creating different pickers kind.
 */
public abstract class BasePickersFactory {

    private static BasePickersFactory factory = null;

    /**
     * Creates instance of date picker.
     */
    @NonNull
    abstract public BaseDateTimePicker createDatePicker(@NonNull FMXNativeActivity activity);

    /**
     * Creates instance of time picker.
     */
    @NonNull
    abstract public BaseDateTimePicker createTimePicker(@NonNull FMXNativeActivity activity);

    /**
     * Creates instance of list picker.
     */
    @NonNull
    abstract public BaseListPicker createListPicker(@NonNull FMXNativeActivity activity);

    /**
     * Returns pickers factory.
     */
    public static BasePickersFactory getFactory() {
        if (factory == null) {
            factory = new DefaultPickersFactory();
        }

        return factory;
    }
}