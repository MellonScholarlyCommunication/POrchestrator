const jsonld = require('jsonld');

import fs from 'fs';
import { JsonLdProcessor } from 'jsonld';
import { json } from 'stream/consumers';

function inlineContext(directory:string , json: any) {
    if (! json['@context']) {
        json['@context'] = [];
    }

    if (typeof json['@context'] === 'string') {
        json['@context'] = [ json['@context'] ];
    }

    const files = fs.readdirSync(directory);

    files.forEach( file => {

            if (! file.match(/\.jsonld$/)) {
                return;
            }

            const contextStr = fs.readFileSync(`${directory}/${file}`,'utf-8');
            const context = JSON.parse(contextStr);

            json['@context'].push(context['@context']);
    });

    return json;
}

async function quadsToJson(inputFile: string, topId: string) {
    let frame   = {
        "@context" : [
            { "ex" : "https://www.example.org/" }
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

    // Inline the contexts to disable network access 
    frame = inlineContext('context', frame);

    console.log(JSON.stringify(frame));

    const nquadStr  = fs.readFileSync(inputFile,'utf-8');
    
    const ld = await jsonld.fromRDF(nquadStr,{ format:'application/n-quads'});

    const json = await jsonld.frame(ld,frame);

    // Set the public context urls
    json['@context'] = [
        "https://www.w3.org/ns/activitystreams",
        "http://purl.org/coar/notify" ,
        { "ex" : "https://www.example.org/"  }
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
