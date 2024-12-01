#!/bin/bash
stack run | tee >(sed -r 's/\x1b\[[0-9;]*m//g' > $1)
