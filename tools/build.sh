#!/bin/bash

set -e

# depend: opencv vtk git cmake make openmpi hdf5-openmpi pugixml android-sdk-platform-tools

pkg_root=$(pwd)
result_dir="$pkg_root/build"
penguin_build_dir="$pkg_root/penguin-stats-recognize-v3/build"
paddle_build_dir="$pkg_root/PaddleOCR/deploy/cpp_infer/build"
maa_build_dir="$pkg_root/MeoAssistantArknights/build"

lib_paddle="$pkg_root/paddle_inference"
lib_opencv="/usr/lib;/usr/lib/cmake/opencv4;/usr/include"

echo "Paddle lib: $lib_paddle"
echo "OpenCV lib: $lib_opencv"
echo
echo "Some dependences will download automatically by PaddleOCR"
echo

# Prepare resource
if [[ ! -d "$lib_paddle" ]]; then
    echo "Extract paddle_inference.tgz"
    tar zxvf "$pkg_root/paddle_inference.tgz" > /dev/null
fi

if [[ ! -d "$result_dir" ]]; then
    mkdir "$result_dir"
fi


# Compile penguin state recongnize
psr_build_filename="libpenguin-stats-recognize.so"
if [[ ! -f "$result_dir/$psr_build_filename" ]]; then

    if [[ -d "$penguin_build_dir" ]]; then
        rm -rf "$penguin_build_dir"
    fi
    mkdir "$penguin_build_dir"
    cd "$penguin_build_dir"
    cmake .. -DOpenCV_DIR=$lib_opencv
    make -j
    cp "$penguin_build_dir/$psr_build_filename" "$result_dir/$psr_build_filename"
    cd "$pkg_root"
else
    echo "$psr_build_filename already existed, skipped."
fi

# Compile paddle ocr
ppocr_build_filename="libppocr.so"
if [[ ! -f "$result_dir/$ppocr_build_filename" ]]; then
    if [[ -d "$paddle_build_dir" ]]; then
        rm -rf "$paddle_build_dir"
    fi
    mkdir "$paddle_build_dir"
    cd "$paddle_build_dir"
    sed -i 's/-o3/-O3/g' ../CMakeLists.txt # fix typo
    cmake .. \
        -DPADDLE_LIB=$lib_paddle \
        -DWITH_MKL=ON \
        -DWITH_GPU=OFF \
        -DWITH_STATIC_LIB=OFF \
        -DWITH_TENSORRT=OFF \
        -DOPENCV_DIR=$lib_opencv \
        -DWITH_STATIC_LIB=OFF \
        -DBUILD_SHARED=ON
    make -j
    cp "$paddle_build_dir/$ppocr_build_filename" "$result_dir/$ppocr_build_filename"
    cd "$pkg_root"
else
    echo "$ppocr_build_filename already existed, skipped."
fi

# Compile MAA
maa_build_filename="libMeoAssistant.so"
if [[ ! -f "$result_dir/$maa_build_filename" ]]; then
    ln -sf "$result_dir/$ppocr_build_filename" "$pkg_root/MeoAssistantArknights/3rdparty/lib/$ppocr_build_filename"
    ln -sf "$result_dir/$psr_build_filename" "$pkg_root/MeoAssistantArknights/3rdparty/lib/$psr_build_filename"

    if [[ ! -d "$maa_build_dir" ]]; then
        rm -rf "$maa_build_dir"
    fi
    cd "$pkg_root/MeoAssistantArknights/tools"
    echo "update resource"
    sh "./update_resource.sh" $maa_build_dir
    cd "$maa_build_dir"

    cmake ..
    make -j
    cp "$maa_build_dir/$maa_build_filename" "$result_dir/$maa_build_filename"
    cp -r "$maa_build_dir/resource" "$result_dir/resource"
fi

if [[ -d "$pkg_root/../lib" ]]; then
  rm -rf "$pkg_root/../lib"
fi

cp -r "$pkg_root/build" "$pkg_root/../lib"

