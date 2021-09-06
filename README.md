# POrchestrator

An Mellon orchestrator component in Prolog. 

Probably all this code could written as an EYE plugin...
Keeping it simple for now.

# Required

- [EYE](http://eulersharp.sourceforge.net)
- [SWIPL](https://www.swi-prolog.org)
- [Node](https://nodejs.org/en/)
- Typescript - `npm install -g typescipt`

# Install

```
npm install
swipl -t 'pack_install(uuid)'
swipl -t 'pack_install(list_util)'
make compile
```

# Usage

```
bin/orchestrator.sh data/coar_offer.jsonld rules/*
```

Check the `output` directory for JSON-LD that is processed by the rule engine.