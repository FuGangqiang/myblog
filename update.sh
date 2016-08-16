#!/bin/bash

DEST=$(pwd)

rm $DEST/blog/*
rm $DEST/media/*
rm $DEST/static/*
rm $DEST/index.html

mdblog build
