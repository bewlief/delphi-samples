package com.embarcadero.firemonkey.dialogs;

import com.embarcadero.firemonkey.FMXNativeActivity;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.text.InputType;
import android.util.DisplayMetrics;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;

import java.util.Objects;

public class FMXDialogHelpers {

    public static void generateAlertDialog(FMXNativeActivity activity,
                                           AlertDialog.Builder builder, String msg, int dlgType,
                                           final String[] captions, final int posButton, final int negButton,
                                           final int neutralButton, final FMXStandardDialog fmxdialog) {
        switch (dlgType) {
            case 0:
            case 1:
                builder.setIcon(android.R.drawable.ic_dialog_alert);
                break;
            case 2:
                builder.setIcon(android.R.drawable.ic_dialog_info);
                break;
            default:
                break;
        }

        if (posButton >= 0)
            builder.setPositiveButton(captions[0],
                    new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int id) {
                            fmxdialog.setModalResult(posButton);
                        }
                    });
        if (negButton >= 0)
            builder.setNegativeButton(captions[1],
                    new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int id) {
                            fmxdialog.setModalResult(negButton);
                        }
                    });
        if (neutralButton >= 0)
            builder.setNeutralButton(captions[2],
                    new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int id) {
                            fmxdialog.setModalResult(neutralButton);
                        }
                    });

        builder.setMessage(msg);
    }

    public static EditText[] generateInputQuery(
            @NonNull Context context, @NonNull AlertDialog.Builder builder,
            String title, String[] prompts, String[] values, String[] captions,
            final FMXStandardDialog fmxdialog) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(builder, "builder");

        builder.setTitle(title);

        LinearLayout layout = new LinearLayout(context);
        layout.setPadding(dpToPx(context, 24), 0, dpToPx(context, 24), 0);
        layout.setOrientation(LinearLayout.VERTICAL);

        TextView[] labels = new TextView[prompts.length];
        EditText[] ValueEdits = new EditText[values.length];

        for (int i = 0; i < ValueEdits.length; i++) {
            int inputType = InputType.TYPE_CLASS_TEXT;
            if (i < labels.length) {
                String labelText = prompts[i];
                labels[i] = new TextView(context);
                if (prompts[i].length() > 0 && prompts[i].charAt(0) < 0x32) {
                    inputType |= InputType.TYPE_TEXT_VARIATION_PASSWORD;
                    labelText = prompts[i].substring(1);
                }
                labels[i].setText(labelText);
                layout.addView(labels[i]);
                ((LinearLayout.LayoutParams) labels[i].getLayoutParams()).topMargin = dpToPx(context, 16);
            }
            ValueEdits[i] = new EditText(context);
            ValueEdits[i].setInputType(inputType);
            ValueEdits[i].getEditableText().append(values[i]);
            if (i == 0) {
                ValueEdits[i].selectAll();
            }
            layout.addView(ValueEdits[i]);
        }
        builder.setView(layout);

        builder.setPositiveButton(captions[0],
                new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int id) {
                        fmxdialog.setModalResult(1); // mrOK
                    }
                });

        builder.setNegativeButton(captions[1],
                new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int id) {
                        fmxdialog.setModalResult(2); // mrCancel
                    }
                });
        return ValueEdits;
    }

    private static int dpToPx(@NonNull Context context, int dp) {
        Objects.requireNonNull(context, "context");

        DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
        return Math.round(dp * (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
    }
}
