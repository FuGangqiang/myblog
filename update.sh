#!/bin/bash

DEST=$(pwd)/_builds

rm $DEST/blog/* -rf
rm $DEST/media/* -rf
rm $DEST/static/* -rf
rm $DEST/index.html

mdblog build
