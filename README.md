# pkgtreediff

[![Hackage](https://img.shields.io/hackage/v/pkgtreediff.svg)](https://hackage.haskell.org/package/pkgtreediff)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pkgtreediff/badge/lts)](http://stackage.org/lts/package/pkgtreediff)
[![Stackage Nightly](http://stackage.org/package/pkgtreediff/badge/nightly)](http://stackage.org/nightly/package/pkgtreediff)
[![Build status](https://secure.travis-ci.org/juhp/pkgtreediff.svg)](https://travis-ci.org/juhp/pkgtreediff)

## Usage example

```bash session
$ pkgtreediff -R https://dl.fedoraproject.org/pub/fedora/linux/updates/{29,30}/Modular/SRPMS/Packages/
+ aopalliance.src  1.0-17.module_f28+3939+dc18cd75
+ apache-commons-cli.src  1.4-4.module_f28+3939+dc18cd75
+ apache-commons-codec.src  1.11-3.module_f28+3939+dc18cd75
+ apache-commons-io.src  2.6-3.module_f28+3939+dc18cd75
+ apache-commons-lang3.src  3.7-3.module_f28+3939+dc18cd75
+ apache-commons-logging.src  1.2-13.module_f28+3939+dc18cd75
+ atinject.src  1-28.20100611svn86.module_f28+3939+dc18cd75
- byteman.src  4.0.5-1.module_2517+4757d934
- c-ares.src  1.15.0-3.module_f29+3987+c0a4da00
+ cdi-api.src  1.2-8.module_f28+3939+dc18cd75
+ community-mysql.src  5.6.39-2.module_f30+2974+f054c829
 cri-o.src: 1.12.6-1.git2f0cb0d.module_f29+3064+a0dd0d52 -> 1.11.8-1.git71cc465.module_f30+2889+23ad9b1c
 cri-o.src: 1.13.0-1.gite8a2525.module_f29+3066+eba77a73 -> 1.12.6-1.git2f0cb0d.module_f30+3063+3774c70c
+ cri-o.src  1.13.0-1.gite8a2525.module_f30+3068+5f412e7f
:
:
 rubygem-pg.src: 1.1.4-3.module_f29+3836+4592e78f -> 1.1.3-1.module_2225+420254da
+ rubygem-pg.src  1.1.4-3.module_f30+4125+c677839c
+ setools.src  4.2.0-1.module_f30+3425+bbab1a14
+ sisu.src  0.3.3-6.module_f28+3939+dc18cd75
+ slf4j.src  1.7.25-4.module_f28+3939+dc18cd75
 stratisd.src: 1.0.4-1.module_f29+4199+c4ae1ed0 -> 1.0.0-1.module_2234+8ab72e74
+ stratisd.src  1.0.0-1.module_2234+9e1d1821
+ stratisd.src  1.0.4-1.module_f30+4201+e7a8c4f2
+ swig.src  3.0.12-24.module_f30+3039+4e7a1425

Summary
Updated: 17
Added: 223
Deleted: 29
Arch changed: 0
Total packages: 300 -> 494
```

## Builds

Builds are available in
[copr](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/)
for Fedora, EPEL, and OpenSuSE
([more details](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/monitor/detailed)).
