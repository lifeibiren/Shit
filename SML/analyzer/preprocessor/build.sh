#!/bin/bash

mllex preprocessor.lex
mlyacc preprocessor.grm
mlton preprocessor.mlb
