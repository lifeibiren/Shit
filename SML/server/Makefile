MLTON_HOME=/usr/lib/mlton
MLTON_INCLUDE=$(MLTON_HOME)/include
MLTON_BIN=/usr/bin/mlton

#MLTON_FLAGS=-profile time -profile-branch true -profile-stack true -profile-val true
CC=gcc
CFLAGS=-I$(MLTON_INCLUDE)
LDFLAGS=-lcrypto -lssl
.PHONY: all clean

all: server

server: server.mlb *.sml
	$(MLTON_BIN) $(MLTON_FLAGS) -output server -link-opt "$(LDFLAGS)" server.mlb

clean:
	rm -f *.o server
