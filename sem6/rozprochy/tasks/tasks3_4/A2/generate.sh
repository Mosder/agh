#!/bin/bash

./clean_gen.sh
mkdir client/gen
mkdir server/src/main/java/gen
protoc -I. --java_out=server/src/main/java --plugin=protoc-gen-grpc-java='/usr/bin/protoc-gen-grpc-java' --grpc-java_out=server/src/main/java steam_sales.proto
.venv/bin/python -m grpc_tools.protoc -I. --python_out=client/gen --pyi_out=client/gen --grpc_python_out=client/gen steam_sales.proto
