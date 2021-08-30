N3 = data/mellon_accept.n3 data/coar_offer.n3

all: $(N3) 

data/%.n3 : data/%.jsonld
	node bin/jsonld_nquads $< > $@

clean:
	rm data/*.n3

logclean:
	rm -f output/appendToLog/* ;
	rm -f output/sendNotification/* 

compile:
	tsc

watch:
	tsc -w