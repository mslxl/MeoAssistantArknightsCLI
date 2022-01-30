#!/bin/bash

set -e

function clone(){
    git clone "$1" "$2"
    cd "$2"
    git checkout $(git tag | tail -n 1)
}

clone "https://github.com/MistEO/MeoAssistantArknights.git" "MeoAssistantArknights"
clone "https://github.com/MistEO/penguin-stats-recognize-v3.git" "penguin-stats-recognize-v3"
wget "https://paddle-inference-lib.bj.bcebos.com/2.2.2/cxx_c/Linux/CPU/gcc8.2_avx_mkl/paddle_inference.tgz" -O "paddle_inference.tgz"
clone "https://github.com/MistEO/PaddleOCR.git" "PaddleOCR"
