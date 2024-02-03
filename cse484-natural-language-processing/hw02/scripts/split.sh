#!/bin/bash

# Define variables
DATA_DIR="../data"
SOURCE_FILE="$DATA_DIR/wiki_00"
SYLLABLE_FILE="$DATA_DIR/wiki_20000_syllable.txt"
SYLLABLE_FILE_TRAIN="$DATA_DIR/wiki_train.txt"
SYLLABLE_FILE_TEST="$DATA_DIR/wiki_test.txt"

# Extract 19000 lines for wiki_train.txt and 1000 lines for wiki_test.txt
head -n 19000 "$SYLLABLE_FILE" > "$SYLLABLE_FILE_TRAIN"
tail -n  1000 "$SYLLABLE_FILE" > "$SYLLABLE_FILE_TEST"