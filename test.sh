#!/bin/bash

curl -X POST http://localhost:12345/INCD \
    -H "Content-Type: application/json" \
    -d '{"data":[{"study":"a", "Et":2, "Nt":9}, {"study":"b", "Et":0, "Nt":4}], "cfg":{"sm":"PLOGIT", "pooling_method":"Inverse", "tau_estimation_method":"DL", "hakn_adjustment":true, "adhoc_hakn":"se"}}'