#!/usr/bin/env bash
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

LIB_PHONE_NUMBER_REPO=$1
LIB_PHONE_NUMBER_REV=$2

echo "Use repo ${LIBPHONE_NUMBER_REPO} and revision ${LIBPHONE_NUMBER_REV}"

install_libphonenumber()
{
  git clone ${LIB_PHONE_NUMBER_REPO}
  pushd libphonenumber
  git checkout ${LIB_PHONE_NUMBER_REV}
  popd

  mkdir -p libphonenumber/cpp/build
  pushd libphonenumber/cpp/build
  export CFLAGS=-fPIC
  export CXXFLAGS=-fPIC
  cmake -DCMAKE_INSTALL_PREFIX:PATH=install  ..
  make
  make install
  popd
}

check_for_package()
{
  if [ "`apt-cache policy $1 | grep "(none)"`" ]; then
    echo "Error: Cannot find $1 - install it with apt-get install $1"
    exit 1;
  fi
}

mkdir -p deps
pushd deps

case $OS in
  Linux)
    case $KERNEL in
      Ubuntu)
        echo "Linux, Ubuntu"

        check_for_package "cmake"
        check_for_package "libprotobuf-dev"
        check_for_package "protobuf-compiler"
        check_for_package "libgtest-dev"
        check_for_package "libre2-1"
        check_for_package "libre2-dev"
        check_for_package "libicu-dev"
        check_for_package "libboost-dev"
        check_for_package "libboost-thread-dev"
        check_for_package "libboost-system-dev"

        install_libphonenumber
        ;;
      *) echo "Your system $KERNEL is not supported"
    esac
    ;;
  *) echo "Your system $OS is not supported"
esac

popd
