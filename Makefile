.PHONY=server
server:
	dune exec chat server

client:
	dune exec chat client

test:
	dune runtest