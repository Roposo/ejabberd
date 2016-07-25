#!/bin/bash

./autogen.sh
if [ "$(uname)" == "Darwin" ]; then
  echo "Configuring for Mac"
  export LDFLAGS="-L/usr/local/opt/openssl/lib -L/usr/local/lib -L/usr/local/opt/expat/lib"
  export CFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
  export CPPFLAGS="-I/usr/local/opt/openssl/include/ -I/usr/local/include -I/usr/local/opt/expat/include"
elif [ "$(uname)" == "Linux" ]; then
  echo "Configuring for Linux"
fi
./configure --prefix=$PWD/installed --enable-mysql
make && make install
