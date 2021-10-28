import * as jsonld from 'jsonld';
import fs from 'fs';

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

            const name = file.replace(/\.jsonld/,'');

            const contextStr = fs.readFileSync(`${directory}/${file}`,'utf-8');
            const context = JSON.parse(contextStr);

            json['@context'] = json['@context'].map( (item:any) => {
                if (typeof item === 'string' && item.indexOf(name) >= 0) {
                    return context['@context']; 
                }
                else {
                    return item;
                }
            });
    });

    return json;
}

async function jsonToQuads(inputFile: string) {
    const jsonStr = fs.readFileSync(inputFile,'utf-8');
    let   json    = JSON.parse(jsonStr);

    json = inlineContext('context',json);

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