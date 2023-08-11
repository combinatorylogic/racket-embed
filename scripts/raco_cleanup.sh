#!/bin/bash

((raco pkg show demo_common | tail -n 1 | grep demo_common) && (raco pkg remove demo_common)) || (exit 0)

