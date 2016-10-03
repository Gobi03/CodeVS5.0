FSCBIN := $(shell dirname `which fsc`)
FSC = $(FSCBIN)/fsc

BASE = Collect.class Escape.class Attack.class
FUNCMAIN = funcForMain.class
FIELD = GameState.class PlayerState.class
SUBFUNC = debugs.class functions.class


.PHONY: all clean
all: Main.class

Main.class: $(BASE)
	$(FSC) Main.scala
$(BASE): $(FUNCMAIN)
	$(FSC) Collect.scala Escape.scala Attack.scala
$(FUNCMAIN): $(FIELD)
	$(FSC) funcForMain.scala
$(FIELD): $(SUBFUNC)
	$(FSC) GameState.scala PlayerState.scala
$(SUBFUNC):
	$(FSC) debugs.scala functions.scala


clean:
	rm -f *.class
