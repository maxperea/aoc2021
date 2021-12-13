CURRENT=13
PART=a

all: build run

run: build
	cat input/$(CURRENT).input | ./current

build:
	ghc -O2 $(CURRENT)$(PART).hs -o current

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a *.hp *.eps *.aux *.latex *.log *.pdf

test: build
	cat input/test.input | ./current
