#!/usr/bin/env bash

file=$1
racket listing.rkt $file ${file%.asm}.lst
