#!/bin/bash

if [ "x$PROTOC" == "x" ]
then
    echo "Set PROTOC to protoc version 2.2.0"
    exit 1
fi

$PROTOC --java_out ../../../../.. jribble.proto
