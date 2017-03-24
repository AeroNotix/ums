#!/bin/bash

kubectl delete pods $(kubectl get pods | tail -n +2 | awk '{{print $1}}' | paste -sd " " -)
