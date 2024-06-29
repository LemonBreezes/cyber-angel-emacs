#!/usr/bin/env sh

pkg install python lzop pigz plzip unrar zip tree python fd ripgrep-all gdb gh openssh git wakatime-cli termux-api nodejs poppler ffmpeg libvterm cmake aspell aspell-en autoconf direnv emacs git mu isync texlive-bin automake

# Directory where the org directory will be linked
if [ -e ~/org ]; then
  echo "Error: ~/org already exists."
else
  ln -s /storage/emulated/0/org ~/org
  echo "Symbolic link created: ~/org -> /storage/emulated/0/org"
fi

# unless emacs/files is symbolically linked to termux home, do that
# The TSC dynamic module is being obtained compiled for X86_64 instead of AARCH64 which is what my Pixel phone supports.
