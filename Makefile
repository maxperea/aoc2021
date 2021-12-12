CURRENT=8
PART=b

all: build run

run: build
	cat input/$(CURRENT).input | ./current

build:
	ghc $(CURRENT)$(PART).hs -o current

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a *.hp *.eps *.aux *.latex *.log *.pdf
