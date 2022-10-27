#!/bin/sh

for l in o z; do
    for v in 35 36; do
        mkdir -p $v/Server/$l
        ( cd $v/Server/$l
          for i in $(findhttp http://download.fedoraproject.org/pub/fedora/linux/releases/$v/Server/x86_64/os/Packages/o); do touch $i; done
        )
    done
done
