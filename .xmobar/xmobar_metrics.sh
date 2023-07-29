#!/bin/bash

output="${1}.shmid"

/home/jp21734/git-repos/github.com/bitterfox/ssmcli/ssmcli_get `cat $output`
