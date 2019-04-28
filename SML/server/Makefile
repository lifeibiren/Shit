MLTON_HOME=/usr/lib/mlton
MLTON_INCLUDE=$(MLTON_HOME)/include
MLTON_BIN=/usr/bin/mlton

CC=gcc
CFLAGS=-I$(MLTON_INCLUDE)
LDFLAGS=-lcrypto -lssl
.PHONY: all clean

all: server

server: aes.mlb aes.sml
	$(MLTON_BIN) -output server -link-opt "$(LDFLAGS)" aes.mlb

clean:
	rm -f *.o
