#!/usr/bin/env sh

mkdir -p /tmp/portage && cd /tmp/portage
wget https://gitweb.gentoo.org/proj/portage.git/snapshot/portage-3.0.30.tar.gz
tar xvf portage-3.0.30.tar.gz -C /tmp/portage --strip-components=1
export PYTHONPATH="/tmp/portage/lib${PYTHONPATH:+:}${PYTHONPATH}"
export PATH="/tmp/portage/bin:${PATH}"
sudo bin/emerge --verbose --oneshot sys-apps/portage
