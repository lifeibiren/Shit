#!/bin/bash

mllex lexer.lex
mlton -output analyzer analyzer.mlb
