import * as jsonld from 'jsonld';
import fs from 'fs';

async function jsonToQuads(inputFile: string) {
    // Inline the contexts to disable network access 
    const asContextStr = 
        fs.readFileSync('context/activitystreams.jsonld','utf-8');
    const notifyContextStr = 
        fs.readFileSync('context/activitystreams.jsonld','utf-8');

    const asContext     = JSON.parse(asContextStr);
    const notifyContext = JSON.parse(notifyContextStr);

    const jsonStr = fs.readFileSync(inputFile,'utf-8');
    const json    = JSON.parse(jsonStr);

    json['@context'] = [
        asContext['@context'],
        notifyContext['@context']
    ];
    
    const nquads  = await jsonld.toRDF(json, {format: 'application/n-quads'});

    return nquads;
}

const args = process.argv.slice(2);

if (args.length == 0) {
    console.error('usage: jsonld_nquads FILE');
    process.exit(1);
}

const inputFile = args[0];

jsonToQuads(inputFile == '-' ? '/dev/stdin' : inputFile).then((result) => {
    console.log(result);
});