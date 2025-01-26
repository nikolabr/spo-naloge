#!/usr/bin/env bash

file=$1
racket asm.rkt $file ${file%.asm}.obj
