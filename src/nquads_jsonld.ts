const jsonld = require('jsonld');

import fs from 'fs';
import { JsonLdProcessor } from 'jsonld';
import { json } from 'stream/consumers';

async function quadsToJson(inputFile: string, topId: string) {
    // Inline the contexts to disable network access 
    const asContextStr = 
                fs.readFileSync('context/activitystreams.jsonld','utf-8');
    const notifyContextStr = 
                fs.readFileSync('context/notify.jsonld','utf-8');

    const asContext     = JSON.parse(asContextStr);
    const notifyContext = JSON.parse(notifyContextStr);

    const frame   = {
        "@context" : [
             asContext['@context'],
             notifyContext['@context']
        ] ,
        "id": topId , 
        "inReplyTo": {
            "@embed": "@never"
        },
        "origin": {
            "@embed": "@always"
        },
        "target": {
            "@embed": "@always"
        }
    };

    const nquadStr  = fs.readFileSync(inputFile,'utf-8');
    
    const ld = await jsonld.fromRDF(nquadStr,{ format:'application/n-quads'});

    const json = await jsonld.frame(ld,frame);

    // Set the public context urls
    json['@context'] = [
        "https://www.w3.org/ns/activitystreams",
        "http://purl.org/coar/notify"
    ];

    return JSON.stringify(json,null,2);
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
