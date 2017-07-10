OUTPUT="../../libsimd-ios.a"
PLATFORM="iPhoneOS"

DEVELOPER_DIR=`xcode-select -print-path`
if [ ! -d $DEVELOPER_DIR ]; then
  echo "Please set up Xcode correctly. '$DEVELOPER_DIR' is not a valid developer tools folder."
  exit 1
fi

SDK_ROOT=$DEVELOPER_DIR/Platforms/$PLATFORM.platform/Developer/SDKs/$PLATFORM.sdk
if [ ! -d $SDK_ROOT ]; then
  echo "The iOS SDK was not found in $SDK_ROOT."
  exit 1
fi

rm armv7.a
rm arm64.a
rm *.o

clang -c -O3 -arch armv7 -isysroot $SDK_ROOT ../Arm32/simd_32.S
ar rcs armv7.a *_32.o
ranlib armv7.a

clang -c -O3 -arch arm64 -isysroot $SDK_ROOT ../Arm64/simd_64.S
ar rcs arm64.a *_64.o
ranlib arm64.a

rm $OUTPUT
lipo -create -arch armv7 armv7.a -arch arm64 arm64.a -output $OUTPUT

rm armv7.a
rm arm64.a
rm *.o