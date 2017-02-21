#!/bin/bash

sudo apt install ghc
sudo apt install cabal-install
cabal update
cabal install base #Aca esta Foreing
cabal install hsc2hs

#Install OpenCV 3
# http://docs.opencv.org/2.4/doc/tutorials/introduction/linux_install/linux_install.html#linux-installation
wget https://github.com/Itseez/opencv/archive/3.1.0.zip
unzip 3.1.0.zip

cd ~/opencv
mkdir release
cd release
cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local ..
make
sudo make install

cabal install vector-space
cabal install allocated-processor
