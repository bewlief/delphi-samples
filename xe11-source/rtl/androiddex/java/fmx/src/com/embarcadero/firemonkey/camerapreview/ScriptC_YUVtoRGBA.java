/*
 * Copyright (C) 2011-2014 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * This file is auto-generated. DO NOT MODIFY!
 */

package com.embarcadero.firemonkey.camerapreview;

import android.os.Build;
import android.os.Process;
import java.lang.reflect.Field;
import android.renderscript.*;
import com.embarcadero.firemonkey.camerapreview.YUVtoRGBABitCode;

public class ScriptC_YUVtoRGBA extends ScriptC {
    private static final String __rs_resource_name = "yuvtorgba";
    // Constructor
    public  ScriptC_YUVtoRGBA(RenderScript rs) {
        super(rs,
              __rs_resource_name,
              YUVtoRGBABitCode.getBitCode32(),
              YUVtoRGBABitCode.getBitCode64());
        __ALLOCATION = Element.ALLOCATION(rs);
        __I32 = Element.I32(rs);
        __U8_4 = Element.U8_4(rs);
    }

    private Element __ALLOCATION;
    private Element __I32;
    private Element __U8_4;
    private FieldPacker __rs_fp_ALLOCATION;
    private FieldPacker __rs_fp_I32;
    private final static int mExportVarIdx_Input = 0;
    private Allocation mExportVar_Input;
    public synchronized void set_Input(Allocation v) {
        setVar(mExportVarIdx_Input, v);
        mExportVar_Input = v;
    }

    public Allocation get_Input() {
        return mExportVar_Input;
    }

    public Script.FieldID getFieldID_Input() {
        return createFieldID(mExportVarIdx_Input, null);
    }

    private final static int mExportVarIdx_Width = 1;
    private int mExportVar_Width;
    public synchronized void set_Width(int v) {
        setVar(mExportVarIdx_Width, v);
        mExportVar_Width = v;
    }

    public int get_Width() {
        return mExportVar_Width;
    }

    public Script.FieldID getFieldID_Width() {
        return createFieldID(mExportVarIdx_Width, null);
    }

    private final static int mExportVarIdx_Height = 2;
    private int mExportVar_Height;
    public synchronized void set_Height(int v) {
        setVar(mExportVarIdx_Height, v);
        mExportVar_Height = v;
    }

    public int get_Height() {
        return mExportVar_Height;
    }

    public Script.FieldID getFieldID_Height() {
        return createFieldID(mExportVarIdx_Height, null);
    }

    //private final static int mExportForEachIdx_root = 0;
    private final static int mExportForEachIdx_ProcessFrameRotated0Degrees = 1;
    public Script.KernelID getKernelID_ProcessFrameRotated0Degrees() {
        return createKernelID(mExportForEachIdx_ProcessFrameRotated0Degrees, 58, null, null);
    }

    public void forEach_ProcessFrameRotated0Degrees(Allocation aout) {
        forEach_ProcessFrameRotated0Degrees(aout, null);
    }

    public void forEach_ProcessFrameRotated0Degrees(Allocation aout, Script.LaunchOptions sc) {
        // check aout
        if (!aout.getType().getElement().isCompatible(__U8_4)) {
            throw new RSRuntimeException("Type mismatch with U8_4!");
        }
        forEach(mExportForEachIdx_ProcessFrameRotated0Degrees, (Allocation) null, aout, null, sc);
    }

    private final static int mExportForEachIdx_ProcessFrameRotated90Degrees = 2;
    public Script.KernelID getKernelID_ProcessFrameRotated90Degrees() {
        return createKernelID(mExportForEachIdx_ProcessFrameRotated90Degrees, 58, null, null);
    }

    public void forEach_ProcessFrameRotated90Degrees(Allocation aout) {
        forEach_ProcessFrameRotated90Degrees(aout, null);
    }

    public void forEach_ProcessFrameRotated90Degrees(Allocation aout, Script.LaunchOptions sc) {
        // check aout
        if (!aout.getType().getElement().isCompatible(__U8_4)) {
            throw new RSRuntimeException("Type mismatch with U8_4!");
        }
        forEach(mExportForEachIdx_ProcessFrameRotated90Degrees, (Allocation) null, aout, null, sc);
    }

    private final static int mExportForEachIdx_ProcessFrameRotated180Degrees = 3;
    public Script.KernelID getKernelID_ProcessFrameRotated180Degrees() {
        return createKernelID(mExportForEachIdx_ProcessFrameRotated180Degrees, 58, null, null);
    }

    public void forEach_ProcessFrameRotated180Degrees(Allocation aout) {
        forEach_ProcessFrameRotated180Degrees(aout, null);
    }

    public void forEach_ProcessFrameRotated180Degrees(Allocation aout, Script.LaunchOptions sc) {
        // check aout
        if (!aout.getType().getElement().isCompatible(__U8_4)) {
            throw new RSRuntimeException("Type mismatch with U8_4!");
        }
        forEach(mExportForEachIdx_ProcessFrameRotated180Degrees, (Allocation) null, aout, null, sc);
    }

    private final static int mExportForEachIdx_ProcessFrameRotated270Degrees = 4;
    public Script.KernelID getKernelID_ProcessFrameRotated270Degrees() {
        return createKernelID(mExportForEachIdx_ProcessFrameRotated270Degrees, 58, null, null);
    }

    public void forEach_ProcessFrameRotated270Degrees(Allocation aout) {
        forEach_ProcessFrameRotated270Degrees(aout, null);
    }

    public void forEach_ProcessFrameRotated270Degrees(Allocation aout, Script.LaunchOptions sc) {
        // check aout
        if (!aout.getType().getElement().isCompatible(__U8_4)) {
            throw new RSRuntimeException("Type mismatch with U8_4!");
        }
        forEach(mExportForEachIdx_ProcessFrameRotated270Degrees, (Allocation) null, aout, null, sc);
    }
}