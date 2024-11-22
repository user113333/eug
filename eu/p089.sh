#!/bin/bash

ORIGINAL=$(cat p089.txt | tr -d '\n' | wc -m)
MINIMAL=$(cat p089.txt | roman -r | roman | tr -d '\n' | wc -m)
echo $((ORIGINAL-MINIMAL))
