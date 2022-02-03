# POrchestrator

An Mellon orchestrator component in Prolog. 

Probably all this code could written as an EYE plugin...
Keeping it simple for now.

# Required

- [EYE](https://github.com/josd/eye/releases/tag/v21.0811.1752) version v21.0811.1752
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