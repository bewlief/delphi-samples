package com.embarcadero.firemonkey.medialibrary;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.embarcadero.firemonkey.medialibrary.MediaImage.ImageStorePlace;

import java.util.Objects;

/**
 * Implementation Media Library Service for Android
 */
@SuppressWarnings("unused")
public class FMXMediaLibrary {

    public static final int ACTION_TAKE_IMAGE_FROM_CAMERA = 1;
    public static final int ACTION_TAKE_IMAGE_FROM_LIBRARY = 2;
    public static final int ACTION_CROP_IMAGE = 3;

    private static final String PHOTO_PATH_STORAGE_KEY = "photo_path";
    private static final String TAG = "MediaLibrary";

    private final Activity activity;
    private MediaImage image = null;
    private Size maxSize = new Size(1024, 1024);
    private boolean isEditable = false;
    private boolean isNeededSavePhotoToGallery = false;
    private FMXMediaLibraryListener listener;

    public FMXMediaLibrary(@NonNull Activity activity) {
        this.activity = Objects.requireNonNull(activity, "activity");
    }

    public static boolean isRequestForTakingImage(int requestCode) {
        return (requestCode == ACTION_TAKE_IMAGE_FROM_CAMERA) || (requestCode == ACTION_TAKE_IMAGE_FROM_LIBRARY) || (requestCode == ACTION_CROP_IMAGE);
    }

    /**
     * Send request to system on open camera.  it requests system camera application to open
     * and after finishing it returns the image through <b>Activity.OnActivityResult</b>.
     *
     * @param maxWidth  is required width size of photo
     * @param maxHeight is required height size of photo
     * @param editable  is used to determine if the user can edit the image before selecting it
     */
    public void takeImageFromCamera(int maxWidth, int maxHeight, boolean editable, boolean needSaveToAlbum) {
        maxSize = new Size(maxWidth, maxHeight);
        isEditable = editable;
        isNeededSavePhotoToGallery = needSaveToAlbum;
        // For this action, we should before taking photo create file in photo directory!
        image = new MediaImage(activity, ImageStorePlace.CAMERA_PHOTO);

        Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
        intent.putExtra(MediaStore.EXTRA_OUTPUT, image.getFileUri());

        activity.startActivityForResult(intent, ACTION_TAKE_IMAGE_FROM_CAMERA);
    }

    /**
     * Send request to system on getting photo from image library. It's request open system iamge library application
     * and after finishing it returns the image through <b>Activity.OnActivityResult</b>.
     *
     * @param maxWidth  is required width size of photo
     * @param maxHeight is required height size of photo
     * @param editable  is used to determine if the user can edit the image before selecting it
     */
    public void takeImageFromLibrary(int maxWidth, int maxHeight, boolean editable) {
        image = null;
        maxSize = new Size(maxWidth, maxHeight);
        isEditable = editable;
        isNeededSavePhotoToGallery = false;
        Intent intent = new Intent(Intent.ACTION_PICK);
        intent.setData(MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
        activity.startActivityForResult(intent, ACTION_TAKE_IMAGE_FROM_LIBRARY);
    }

    public void handleTakingPhotoRequest(@Nullable Intent data, int requestCode) {
        switch (requestCode) {
            case ACTION_TAKE_IMAGE_FROM_CAMERA:
                handleCameraPhoto();
                break;

            case ACTION_TAKE_IMAGE_FROM_LIBRARY:
                handleLibraryPhoto(data);
                break;

            case ACTION_CROP_IMAGE:
                handleCrop();
                break;

            default:
                throw new IllegalStateException(String.format("Unknown request code: requestCode=%d", requestCode));
        }
    }

    private void postHandleCameraPhoto() {
        image.loadFromFile(maxSize);
        try {
            image.normalizeOrientation();
            image.saveToFile();
            if (isNeededSavePhotoToGallery) {
                image.addPhotoToGallery();
            }

            notifySuccess(ACTION_TAKE_IMAGE_FROM_CAMERA);
        } finally {
            image.unloadBitmap();
        }
        if (!isNeededSavePhotoToGallery) {
            image.remove();
        }
    }

    private void handleCameraPhoto() {
        if (!image.isFileExists()) {
            Log.w(TAG, "Could not return photo from camera to native side. Could not find the photo file.");
            return;
        }

        if (isEditable) {
            boolean isSuccess = image.cropImage(image.getFileUri());
            // We ignore image cropping, since this step cannot be processed on this device.
            if (!isSuccess) {
                Log.i(TAG, "Photo cropping is being skipped as it is not available on this device.");
                postHandleCameraPhoto();
            }
        } else {
            postHandleCameraPhoto();
        }
    }

    private void postHandleLibraryPhoto(@NonNull Uri uri) {
        Objects.requireNonNull(uri, "uri");

        try {
            image = MediaImage.loadFromUri(activity, uri, maxSize);
            image.normalizeOrientation();
            image.saveToFile();

            notifySuccess(ACTION_TAKE_IMAGE_FROM_LIBRARY);
        } finally {
            image.unloadBitmap();
        }
    }

    private void handleLibraryPhoto(@NonNull Intent data) {
        Objects.requireNonNull(data, "data");

        Uri uri = data.getData();
        if (uri == null) {
            Log.w(TAG, "Could not handle the photo from the Gallery application as the received intent does not have the photo URI.");
            return;
        }

        if (isEditable) {
            image = new MediaImage(activity);
            boolean isSuccess = image.cropImage(uri);
            // We ignore image cropping, since this step cannot be processed on this device.
            if (!isSuccess) {
                Log.i(TAG, "Photo cropping is being skipped as it is not available on this device.");
                postHandleLibraryPhoto(uri);
            }
        } else {
            postHandleLibraryPhoto(uri);
        }
    }

    private void handleCrop() {
        if (image.isFileExists()) {
            image.loadFromFile(maxSize);
            try {
                image.normalizeOrientation();
                image.saveToFile();

                notifySuccess(ACTION_CROP_IMAGE);
            } finally {
                image.unloadBitmap();
            }
        }
    }

    @Nullable
    public String getLastPhotoName() {
        return (image == null) ? null : image.getFileName();
    }

    public void onSaveInstanceState(@NonNull Bundle outState) {
        Objects.requireNonNull(outState, "outState");

        if (image != null) {
            outState.putString(PHOTO_PATH_STORAGE_KEY, image.getFileName());
        }
    }

    public void onRestoreInstanceState(@NonNull Bundle savedInstanceState) {
        Objects.requireNonNull(savedInstanceState, "savedInstanceState");

        String photoPath = savedInstanceState.getString(PHOTO_PATH_STORAGE_KEY);
        if (photoPath != null) {
            if (image == null) {
                image = new MediaImage(activity);
            }
            image.loadFromFile(photoPath, maxSize);
        }
    }

    public void setListener(@Nullable FMXMediaLibraryListener listener) {
        this.listener = listener;
    }

    private void notifySuccess(int actionCode) {
        if (listener != null) {
            listener.onMediaLibraryAccept(actionCode);
        }
    }
}
