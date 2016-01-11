#!/usr/bin/env bash
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

LIB_PHONE_NUMBER_REPO=$1
LIB_PHONE_NUMBER_REV=$2

log()
{
  echo ":: $1"
}

error()
{
  echo "[ERROR] $1"
  exit 1
}

install_libphonenumber()
{
  log "Cloning Libphonenumber"
  git clone ${LIB_PHONE_NUMBER_REPO}
  pushd libphonenumber > /dev/null
  git checkout ${LIB_PHONE_NUMBER_REV}
  popd > /dev/null

  mkdir -p libphonenumber/cpp/build
  pushd libphonenumber/cpp/build > /dev/null
  export CFLAGS=-fPIC
  export CXXFLAGS=-fPIC
  log "Building Libphonenumber"
  cmake -DCMAKE_INSTALL_PREFIX:PATH=install  ..
  make
  make install
  popd > /dev/null
}

check_for_package()
{
  log "Checking for $1"
  if [ "`apt-cache policy $1 | grep "(none)"`" ]; then
    error "Cannot find $1 - install it with apt-get install $1"
    exit 1;
  else
    log "Found $1"
  fi

  log ""
}

log "Use repo ${LIB_PHONE_NUMBER_REPO} and revision ${LIB_PHONE_NUMBER_REV}"

mkdir -p deps
pushd deps > /dev/null

log "Detecting OS"

case $OS in
  Linux)
    case $KERNEL in
      Ubuntu)
        log "Linux, Ubuntu"

        check_for_package "git"
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
      *) error "Your system $KERNEL is not supported"
    esac
    ;;
  *) error "Your system $OS is not supported"
esac

popd
