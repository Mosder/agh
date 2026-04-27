#!/bin/bash

./clean_gen.sh
mkdir client/gen
slice2java --output-dir server/src/main/java slice/objects.ice
.venv/bin/slice2py --output-dir client/gen slice/objects.ice
