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

import android.app.Activity;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.media.ExifInterface;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.FileProvider;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;

@SuppressWarnings("WeakerAccess")
class MediaImage {

    private static final String TAG = "MediaImage";
    private static final String JPEG_FILE_PREFIX = "IMG_";
    private static final String JPEG_FILE_SUFFIX = ".jpg";

    @NonNull
    private final Activity activity;

    @Nullable
    private File imageFile = null;

    @Nullable
    private Bitmap bitmap = null;

    private boolean isFileChanged;

    public enum ImageStorePlace {
        /**
         * Store image bitmap in Photo Album
         */
        CAMERA_PHOTO,

        /**
         * Store image bitmap in temporary cache directory
         */
        CACHE
    }

    /**
     * Load bitmap from web (download image or open from picassa album) with required size.
     * <p/>
     * If bitmap size is bigger a required size, then bitmap will be stretched to required size with storing proportion.
     *
     * @param imageUri uri on image file
     * @param requiredSize required width and height of result bitmap
     */
    @NonNull
    public static MediaImage loadFromUri(@NonNull Activity activity, @NonNull Uri imageUri, @NonNull Size requiredSize) {
        Objects.requireNonNull(activity, "activity");
        Objects.requireNonNull(imageUri, "imageUri");
        Objects.requireNonNull(requiredSize, "requiredSize");

        MediaImage mediaImage = new MediaImage(activity, ImageStorePlace.CACHE);

        String imageFileName = mediaImage.getFileNameFromURI(imageUri);
        if (isContentImage(imageFileName) || isHttpImage(imageFileName)) {
            mediaImage.loadFromWeb(imageUri, requiredSize);
        } else {
            mediaImage.loadFromFile(imageFileName, requiredSize);
        }
        mediaImage.saveToTempFile();
        return mediaImage;
    }

    public MediaImage(@NonNull Activity activity) {
        this.activity = Objects.requireNonNull(activity, "activity");
        this.isFileChanged = false;
    }

    public MediaImage(@NonNull Activity activity, @NonNull ImageStorePlace imageType) {
        this.activity = Objects.requireNonNull(activity, "activity");
        this.isFileChanged = false;
        this.imageFile = imageType == ImageStorePlace.CAMERA_PHOTO ? createPhotoFile() : createTempFile();
    }

    public void remove() {
        if (isFileExists()) {
            imageFile.delete();
        }
    }

    /**
     * Return absolute fine name for encapsulated image.
     * @return absolute file name
     */
    @NonNull
    public String getFileName() {
        return imageFile.getAbsolutePath();
    }

    @NonNull
    public Uri getFileUri() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            String authority = activity.getPackageName() + ".fileprovider";
            return FileProvider.getUriForFile(activity, authority, imageFile);
        }
        return Uri.fromFile(imageFile);
    }

    public boolean isFileExists() {
        return (imageFile != null) && (imageFile.exists());
    }

    public boolean isImageLoaded() {
        return bitmap != null;
    }

    /**
     * Return rotation angle, based on orientation camera, when image was taken.
     *
     * @return Degrees
     */
    public int getRotationAngle() {
        int rotationInDegrees = 0;
        try {
            ExifInterface exif = new ExifInterface(getFileName());
            int orientation = exif.getAttributeInt(ExifInterface.TAG_ORIENTATION, ExifInterface.ORIENTATION_NORMAL);
            rotationInDegrees = exifToDegrees(orientation);
        } catch (IOException e) {
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
        return rotationInDegrees;
    }

    /**
     * Rotate the image for correct viewing of the photo (Based on EXIF information about orientation of photo)
     */
    public void normalizeOrientation() {
        int angle = getRotationAngle();
        if (angle != 0) {
            Matrix matrix = new Matrix();
            matrix.preRotate(angle);
            Bitmap rotatedBitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix,
                                                 true);
            // We should release old bitmap memory.
            unloadBitmap();
            bitmap = rotatedBitmap;
            isFileChanged = true;
        }
    }

    /**
     * Adds photo to Gallery application.
     */
    public void addPhotoToGallery() {
        Intent intent = new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE);
        intent.setData(getFileUri());

        activity.sendBroadcast(intent);
    }

    /**
     * Save picture to file only if bitmap of picture was changed
     */
    public void saveToFile() {
        if (!isFileChanged) {
            return;
        }

        try {
            FileOutputStream out = new FileOutputStream(getFileName());
            bitmap.compress(Bitmap.CompressFormat.JPEG, 100, out);
            out.close();
            isFileChanged = false;
        } catch (Exception e) {
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
    }

    /**
     * Save picture to new temporary file
     */
    public void saveToTempFile() {
        imageFile = createTempFile();
        isFileChanged = true;
        saveToFile();
    }

    /**
     * Load bitmap from file with required size. If bitmap size is bigger a required size,
     * then bitmap will stretch to required size with storing proportion.
     *
     * @param requiredSize required width and height of result bitmap
     */
    public void loadFromFile(@NonNull Size requiredSize) {
        Objects.requireNonNull(requiredSize, "requiredSize");

        loadFromFile(getFileName(), requiredSize);
    }

    public void loadFromFile(@NonNull String fileName, @NonNull Size requiredSize) {
        Objects.requireNonNull(requiredSize, "requiredSize");
        Objects.requireNonNull(fileName, "fileName");

        File fileTmp = new File(fileName);
        unloadBitmap();
        if (fileTmp.exists()) {

            imageFile = fileTmp;
            // Determine size of loaded image
            // First decode with inJustDecodeBounds=true to check dimensions
            BitmapFactory.Options options = new BitmapFactory.Options();
            options.inJustDecodeBounds = true;
            BitmapFactory.decodeFile(fileName, options);

            // Calculate inSampleSize
            options.inSampleSize = calculateInSampleSize(options.outWidth, options.outHeight, requiredSize);
            options.inJustDecodeBounds = false;
            options.inPurgeable = true;

            // Decode bitmap with inSampleSize set
            bitmap = BitmapFactory.decodeFile(fileName, options);
            if (options.inSampleSize > 1) {
                isFileChanged = true;
            }
        } else
        {
            bitmap = null;
            isFileChanged = false;
        }
    }

    private void loadFromWeb(Uri fileUri, @NonNull Size requiredSize) {
        Objects.requireNonNull(requiredSize, "requiredSize");

        try {
            InputStream is;
            String fileName = getFileNameFromURI(fileUri);

            if (isContentImage(fileName)) {
                is = activity.getContentResolver().openInputStream(Uri.parse(fileName));
            } else {
                is = new URL(fileName).openStream();
            }

            OutputStream os = new FileOutputStream(imageFile);
            try {
                Utils.copyStream(is, os);
            } finally {
                os.close();
            }
            loadFromFile(imageFile.getAbsolutePath(), requiredSize);
        } catch (Exception e) {
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
    }

    /**
     * Release native bitmap instance for release memory
     */
    public void unloadBitmap() {
        if (bitmap != null) {
            bitmap.recycle();
            bitmap = null;
        }
    }

    /* Private helpers */

    /**
     * Create temporary file in application cache directory.
     */
    @Nullable
    private File createTempFile() {
        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date());
        String imageFileName = JPEG_FILE_PREFIX + timeStamp;
        File fileTemp;
        try {
            fileTemp = File.createTempFile(imageFileName, JPEG_FILE_SUFFIX);
            fileTemp.deleteOnExit();
        } catch (IOException e) {
            fileTemp = null;
            Log.e(TAG, e.getMessage(), e);
            e.printStackTrace();
        }
        return fileTemp;
    }

    /**
     * Create empty size into device photo album.
     */
    @NonNull
    private File createPhotoFile() {
        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.US).format(new Date());
        String imageFileName = JPEG_FILE_PREFIX + timeStamp + JPEG_FILE_SUFFIX;
        File photosDir = Utils.getPhotosDir();
        return new File(photosDir, imageFileName);
    }

    private int calculateInSampleSize(final int width, final int height, final Size reqSize) {
        // Attention! Note: A power of two value is calculated because the decoder uses
        // a final value by rounding down to the nearest power of two, as per the inSampleSize documentation.

        int inSampleSize = 1;

        if (height > reqSize.getHeight() || width > reqSize.getWidth()) {
            // Calculate the largest inSampleSize value that is a power of 2 and keeps both
            // height and width larger than the requested height and width.
            while ((height / inSampleSize) > reqSize.getHeight() || (width / inSampleSize) > reqSize.getWidth()) {
                inSampleSize *= 2;
            }
        }
        return inSampleSize;
    }

    /**
     * Returns absolute file name for URI, which refers to image in Media Library or in Picasa Album.
     */
    @NonNull
    private String getFileNameFromURI(Uri fileUri) {
        // some devices (OS versions return an URI of com.android instead of com.google.android
        if ((fileUri != null) && fileUri.toString().startsWith("content://com.android.gallery3d.provider")) {
            // use the com.google provider, not the com.android provider.
            fileUri = Uri.parse(fileUri.toString().replace("com.android.gallery3d", "com.google.android.gallery3d"));
        }

        String filePath = null;
        String[] projection = { MediaStore.Images.Media.DATA };
        Cursor cursor = activity.getContentResolver().query(fileUri, projection, null, null, null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    int column_index_data = cursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA);
                    if ((column_index_data != -1)) {
                        filePath = cursor.getString(column_index_data);
                    }
                }
            } finally {
                cursor.close();
            }
        }
        if (filePath == null) {
            filePath = fileUri.toString();
        }
        return filePath;
    }

    /**
     * Convert orientation from EXIF of bitmap file to angle in degrees.
     *
     * @param exifOrientation is orientation from EXIF information of bitmap file
     */
    private static int exifToDegrees(int exifOrientation) {
        switch(exifOrientation) {
            case ExifInterface.ORIENTATION_ROTATE_90:
                return 90;
            case ExifInterface.ORIENTATION_ROTATE_180:
                return 180;
            case ExifInterface.ORIENTATION_ROTATE_270:
                return 270;
            default:
                return 0;
        }
    }

    private static boolean isContentImage(@Nullable String str) {
        return str != null && str.startsWith("content://");
    }

    private static boolean isHttpImage(@Nullable String str) {
        return str != null && (str.startsWith("http://") || str.startsWith("https://"));
    }

    public boolean cropImage(Uri selectedUri) {
        Intent intent = new Intent("com.android.camera.action.CROP");
        File tempFile = createPhotoFile();
        if (selectedUri == null) {
            selectedUri = getFileUri();
        }
        intent.setDataAndType(selectedUri, "image/*");
        intent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
        intent.putExtra("return_data", false);
        intent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(tempFile));
        if (intent.resolveActivity(activity.getPackageManager()) == null) {
            Log.i(TAG, "Could not find an activity to crop the photo.");
            return false;
        }
        imageFile = tempFile;
        activity.startActivityForResult(intent, FMXMediaLibrary.ACTION_CROP_IMAGE);
        return true;
    }
}
