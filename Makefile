N3 = data/mellon_accept.n3 data/coar_offer.n3

all: $(N3) 

data/%.n3 : data/%.jsonld
	node bin/jsonld_nquads $< > $@

clean:
	rm data/*.n3

cleanlog:
	rm -f output/appendToLog/* ;
	rm -f output/sendNotification/* 

cache:
	wget -O context/notify.json https://notify.coar-repositories.org/schema/notify.json ;
	wget -O context/activitystreams.jsonld https://www.w3.org/ns/activitystreams.jsonld
compile:
	tsc

watch:
	tsc -w