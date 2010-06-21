# $Id$

CC = gcc
CFLAGS = -g -O2 -Wall 
CPPFLAGS = -I/usr/local/ssl/include -g -O2 -Wall 
LDFLAGS = 
LIBS = 

ASN_FLAGS = -bber_bin +optimize +driver

ERLANG_CFLAGS = -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include -I/usr/local/lib/erlang/usr/include
ERLANG_LIBS = -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib -lerl_interface -lei

EFLAGS += -I ..
EFLAGS += -pz ..

# make debug=true to compile Erlang module with debug informations.
ifdef debug
	EFLAGS+=+debug_info +export_all
endif

OUTDIR = ..
SOURCES = $(wildcard *.erl) ELDAPv3.erl
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))


all:    $(BEAMS) ELDAPv3.beam

ELDAPv3.beam: ELDAPv3.erl

ELDAPv3.erl:       ELDAPv3.asn
	/usr/local/bin/erlc $(ASN_FLAGS) -W $(EFLAGS) $<

$(OUTDIR)/%.beam:	%.erl ELDAPv3.erl
	/usr/local/bin/erlc -W $(EFLAGS) -o $(OUTDIR) $<

clean:
	rm -f ELDAPv3.asn1db
	rm -f ELDAPv3.erl
	rm -f ELDAPv3.hrl
	rm -f ELDAPv3.beam
	rm -f $(BEAMS)

distclean: clean
	rm -f Makefile

TAGS:
	etags *.erl

