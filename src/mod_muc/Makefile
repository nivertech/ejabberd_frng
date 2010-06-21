# $Id$

CC = gcc
CFLAGS = -g -O2 -Wall 
CPPFLAGS = -I/usr/local/ssl/include -g -O2 -Wall 
LDFLAGS = 
LIBS = 

ERLANG_CFLAGS = -I/usr/local/lib/erlang/lib/erl_interface-3.6.5/include -I/usr/local/lib/erlang/usr/include
ERLANG_LIBS = -L/usr/local/lib/erlang/lib/erl_interface-3.6.5/lib -lerl_interface -lei

EFLAGS += -I ..
EFLAGS += -pz ..

# make debug=true to compile Erlang module with debug informations.
ifdef debug
	EFLAGS+=+debug_info +export_all
endif

ifeq (true, false)
	EFLAGS+=-DNO_TRANSIENT_SUPERVISORS
endif

OUTDIR = ..
SOURCES = $(wildcard *.erl)
BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))


all:    $(BEAMS)

$(OUTDIR)/%.beam:       %.erl
	/usr/local/bin/erlc -W $(EFLAGS) -o $(OUTDIR) $<

clean:
	rm -f $(BEAMS)

distclean: clean
	rm -f Makefile

TAGS:
	etags *.erl

