
all:
	@echo clean

test:
	@sbt test

formal:
	@sbt "testOnly -- -DFORMAL=1"

.PHONY: estimator
estimator:
	@sbt "runMain estimate.LPECompEstimateCR"

clean:
	rm -f *.anno.json
	rm -f *.fir
	rm -f *.v
