
clean:
	rm -f output/appendToLog/* ;
	rm -f output/sendNotification/* 

cache:
	wget -O context/notify.json https://notify.coar-repositories.org/schema/notify.json ;
	wget -O context/activitystreams.jsonld https://www.w3.org/ns/activitystreams.jsonld

compile:
	tsc

watch:
	tsc -w

eyetest:
	eye --pass --quiet --nope data/coar_offer.n3 rules/*

demo:
	 prolog/orchestrator.pl data/coar_offer.jsonld rules/*