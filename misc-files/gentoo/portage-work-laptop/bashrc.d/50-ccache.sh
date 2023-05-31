#!/bin/bash
# (C) Martin V\"ath <martin at mvath.de>
# SPDX-License-Identifier: GPL-2.0-only

# Portage explicitly unsets all CCACHE_* variables in each phase.
# Therefore, we save them to BASHRCD_CCACHE_* in the setup phase;
# in all later phases, we restore CCACHE_* from these variables
CcacheSetup() {
	local i
	: ${CCACHE_BASEDIR=${PORTAGE_TMPDIR:-/var/tmp}/portage}
	: ${CCACHE_SLOPPINESS='file_macro,time_macros,include_file_mtime,include_file_ctime,file_stat_matches,pch_defines'}
	: ${CCACHE_COMPRESS=true}
	if BashrcdTrue $USE_NONGNU && BashrcdTrue $CCACHE_CPP2_OPTIONAL
	then	: ${CCACHE_CPP2=true}
	fi
	# Default to NOHASHDIR unless contrary is specified
	BashrcdTrue "${CCACHE_HASHDIR-}" || CCACHE_NOHASHDIR=true
	for i in ${!CCACHE_*}
	do	if eval "BashrcdTrue \$$i"
		then	eval BASHRCD_$i=\$$i
			export $i
		else	unset $i
		fi
	done
CcacheRestore() {
	local i j
	unset ${!CCACHE_*}
	for i in ${!BASHRCD_CCACHE_*}
	do	j=${i##BASHRCD_}
		eval $j=\$$i
		export $j
	done
}
}

CcacheRestore() {
:
}

# Register CcacheRestore before CcacheSetup to save time in setup phase
BashrcdPhase all CcacheRestore
BashrcdPhase setup CcacheSetup
