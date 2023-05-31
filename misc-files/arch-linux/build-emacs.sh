#!/bin/env bash
export CARCH="x86_64"
export CHOST="x86_64-pc-linux-gnu"
export CFLAGS="-Ofast -march=skylake -ftracer -fno-finite-math-only -fno-signed-zeros -fno-trapping-math -fno-stack-protector -floop-nest-optimize"
export CXXFLAGS=$CFLAGS
export RUSTFLAGS="-C opt-level=2 -C target-cpu=native"
export MAKEFLAGS="-j$(nproc) NATIVE_FULL_AOT=1"
export LDFLAGS="-Wl,-O1 -Wl,--as-needed -Wl,--sort-common"
export CC="/usr/bin/gcc"
export LD="/usr/bin/lld"
./autogen.sh git
./configure --prefix=/usr --sysconfdir=/etc --libexecdir=/usr/lib --localstatedir=/var --mandir=/usr/share/man --with-gameuser=:games --without-selinux --with-modules --without-libotf --without-m17n-flt --without-toolkit-scroll-bars --without-gconf --without-gsettings --with-imagemagick --with-native-compilation --with-mailutils --with-xinput2  --with-x-toolkit=gtk3 --with-xwidgets --with-sound=no --without-gpm --without-compress-install --program-transform-name=s/\([ec]tags\)/\1.emacs/
make NATIVE_FULL_AOT=1 -j$(nproc) -l$(nproc)
