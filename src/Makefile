# Build Lisp source files corresponding to protocol buffer messages.
# Redefine PROTOC if you have installed the protocol buffer compiler
# elsewhere.

PROTOC = $(HOME)/local/software/package/google-protobuf.download/src/protoc

%.lisp: %.proto
	$(PROTOC) --proto_path=. --lisp_out=. $<

PROTO_IMPLS = tfnsw-gtfs-realtime.lisp

.PHONY: all clean

all: $(PROTOC) $(PROTO_IMPLS)

clean:
	rm -f $(PROTO_IMPLS)
