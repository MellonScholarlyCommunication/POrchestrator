# POrchestrator

An Mellon orchestrator component in Prolog. 

Probably this can whole be written as an EYE plugin. 
Keeping it simple for now.

# Required

- [EYE](http://eulersharp.sourceforge.net)
- [SWIPL](https://www.swi-prolog.org)
- Node

# Usage

```
make compile
prolog/orchestrator.pl data/FILE.jsonld rules/*
```

Check the `output` directory for JSON-LD that is processed by the rule engine.