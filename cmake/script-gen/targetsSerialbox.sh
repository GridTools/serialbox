#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

$DIR/targetSerialboxCoreShared.sh
$DIR/targetSerialboxCShared.sh
$DIR/targetSerialboxFortranShared.sh
