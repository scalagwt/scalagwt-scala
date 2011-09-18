#!/bin/bash

if [ "x$PROTOC" == "x" ]
then
    echo "Set PROTOC to protoc version 2.4.1"
    exit 1
fi

$PROTOC --java_out ../../../../.. jribble.proto
