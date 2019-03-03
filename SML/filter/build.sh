#!/bin/bash

mllex lexer.lex
mlyacc parser.grm
mlton -output filter filter.mlb
