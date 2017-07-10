LOCAL_PATH:= $(call my-dir)/../../Arm32
include $(CLEAR_VARS)
 
LOCAL_MODULE    := simd-android
LOCAL_SRC_FILES := simd_32.S
 
include $(BUILD_STATIC_LIBRARY)
