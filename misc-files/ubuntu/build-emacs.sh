sudo apt install -y autoconf automake autotools-dev bsd-mailx build-essential \
    diffstat gnutls-dev imagemagick libasound2-dev libc6-dev libdatrie-dev \
    libdbus-1-dev libgconf2-dev libgif-dev libgnutls28-dev libgpm-dev libgtk2.0-dev \
    libgtk-3-dev libice-dev libjpeg-dev liblockfile-dev liblqr-1-0 libm17n-dev \
    libmagickwand-dev libncurses5-dev libncurses-dev libotf-dev libpng-dev \
    librsvg2-dev libsm-dev libthai-dev libtiff5-dev libtiff-dev libtinfo-dev libtool \
    libx11-dev libxext-dev libxi-dev libxml2-dev libxmu-dev libxmuu-dev libxpm-dev \
    libxrandr-dev libxt-dev libxtst-dev libxv-dev quilt sharutils texinfo xaw3dg \
    xaw3dg-dev xorg-dev xutils-dev zlib1g-dev libjansson-dev libxaw7-dev \
    libselinux1-dev libmagick++-dev gir1.2-javascriptcoregtk-4.0 \
    gir1.2-webkit2-4.0 libglvnd-core-dev libenchant-2-dev libicu-le-hb-dev \
    libidn2-0-dev libjavascriptcoregtk-4.0-dev liboss4-salsa2 libsoup2.4-dev \
    libsystemd-dev libwebkit2gtk-4.0-dev libx11-xcb-dev libxcb-dri2-0-dev \
    libxcb-dri3-dev libxcb-glx0-dev libxcb-present-dev libxshmfence-dev \
    libxcb-composite0-dev libxcb-damage0-dev libxcb-randr0-dev libxcb-xinput-dev \
    libxcb-xinerama0-dev mailutils
sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt install gcc g++ libgccjit-11-dev libjansson4 libjansson-dev
rm -rf ~/src/emacs
git clone https://git.savannah.gnu.org/git/emacs.git ~/src/emacs
cd ~/src/emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
export CFLAGS="-Ofast -march=native -ftracer -fno-finite-math-only -fno-signed-zeros -fno-trapping-math -fno-stack-protector -floop-nest-optimize"
export CXXFLAGS=$CFLAGS
export RUSTFLAGS="-C opt-level=2 -C target-cpu=native"
export LDFLAGS="-Wl,-O1 -Wl,--as-needed -Wl,--sort-common"
./autogen.sh 
./configure --with-json --prefix=/usr --sysconfdir=/etc --libexecdir=/usr/lib --localstatedir=/var --mandir=/usr/share/man --with-gameuser=:games --without-selinux --with-modules --without-libotf --without-m17n-flt --without-toolkit-scroll-bars --without-gconf --without-gsettings --with-imagemagick --with-native-compilation --with-mailutils --with-xinput2  --with-x-toolkit=gtk3 --with-xwidgets --with-sound=no --without-gpm --without-compress-install --program-transform-name=s/\([ec]tags\)/\1.emacs/
make -j$(nproc)
sudo make install
