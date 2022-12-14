#!/bin/sh
set -xe

BASEDIR=$(dirname "$0")
PYVER="$(python3 --version | sed 's/.*\(3\.[0-9]*\).*/\1/')"

if [ -z "$CONDA_PREFIX" ]
then
	case $(uname) in
		Darwin)	KERNELDIR=~/Library/Jupyter/kernels ;;
		Linux)	KERNELDIR=~/.local/share/jupyter/kernels ;;
		*)	exit 1
	esac
  echo 1
	SITEDIR="$(python3 -m site --user-site)" || true
  echo 2
	CONDIR="$HOME/anaconda3/lib/python$PYVER/site-packages"
  echo 3
else
	KERNELDIR="$CONDA_PREFIX"/share/jupyter/kernels
	SITEDIR="$CONDA_PREFIX/lib/python$PYVER"
	CONDIR="$SITEDIR"/site-packages
fi

mkdir -p "$KERNELDIR"
cp -r "$BASEDIR"/dyalog-kernel "$KERNELDIR"/

echo $SITEDIR
mkdir -p "$SITEDIR"
cp -r "$BASEDIR"/dyalog_kernel "$SITEDIR"/

if [ -d "$CONDIR" ] ; then
	cp -r "$BASEDIR"/dyalog_kernel "$CONDIR"/
fi
