/**
 * {*******************************************************}
 * {                                                       }
 * {        Delphi FireMonkey Media Library Service        }
 * {                                                       }
 * {   Implementation Media Library Service for Android    }
 * {                                                       }
 * { Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
 * {                                                       }
 * {*******************************************************}
 */
package com.embarcadero.firemonkey.medialibrary;

class Size {

    private int width = 0;
    private int height = 0;

    public Size(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    public boolean isEmpty() {
        return (height <= 0) || (width <= 0);
    }
}
