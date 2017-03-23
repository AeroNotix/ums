#!/bin/bash

kubectl logs -f $(kubectl get pods | awk "NR==$(echo $1)+1 {{print \$1}}")
