#!/bin/bash

echo ''
echo 'Produce serialized data'
./main_producer

echo ''
echo 'Consume serialized data without pertubation'
./main_consumer

echo ''
echo 'Consume serialized data WITH pertubation'
./main_consumer_perturb
