const jsonld = require('jsonld');

import fs from 'fs';
import { JsonLdProcessor } from 'jsonld';
import { json } from 'stream/consumers';

async function quadsToJson(inputFile: string, topId: string) {
    const frame   = {
        "@context" : [
             "https://www.w3.org/ns/activitystreams",
             "http://purl.org/coar/notify"
        ] ,
        "id": topId
    };

    const nquadStr  = fs.readFileSync(inputFile,'utf-8');
    
    const ld = await jsonld.fromRDF(nquadStr,{ format:'application/n-quads'});

    const json = await jsonld.frame(ld,frame);

    return JSON.stringify(json);
}

const args = process.argv.slice(2);

if (args.length != 2 ) {
    console.error('usage: jsonld_nquads FILE ID');
    process.exit(1);
}

const inputFile = args[0];
const topId     = args[1];

quadsToJson(inputFile == "-" ? '/dev/stdin' : inputFile,topId).then((result) => {
    console.log(result);
});
