# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#using-make
# all commands to be called from within nix-shell

HC      = ghc
HC_OPTS = -cpp -outputdir dist/build -odir dist/build -hidir dist/build -stubdir dist/build -i -idist/build -isrc -iapp -idist/build/autogen -idist/build/global-autogen -Idist/build/autogen -Idist/build/global-autogen -Idist/build $(EXTRA_HC_OPTS) 

SRCS = $(shell find src -name '*.hs')
OBJS = $(patsubst %.c,%.o, ${SRCS})
# TODO test program

studious-telegram : $(OBJS) app/Main.o
	rm -f $@
	$(HC) -o $@ $(HC_OPTS) $(OBJS)

%.o : %.hs
	$(HC) -c $< $(HC_OPTS)

# Standard suffix rules
%.hi: %.o
	@:

depends.mk: $(SRCS)
	ghc -M -dep-makefile $@ -dep-suffix '' $(SRCS)
include depends.mk

default.nix: package.yaml
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'

shell: default.nix
	nix-shell --command "export PS1='[make -M] $(value PS1)'; return"

clean:
	rm -r dist
	rm studious-telegram
