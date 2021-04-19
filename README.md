# pkgtreediff

[![Hackage](https://img.shields.io/hackage/v/pkgtreediff.svg)](https://hackage.haskell.org/package/pkgtreediff)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pkgtreediff/badge/lts)](http://stackage.org/lts/package/pkgtreediff)
[![Stackage Nightly](http://stackage.org/package/pkgtreediff/badge/nightly)](http://stackage.org/nightly/package/pkgtreediff)

`pkgtreediff` compares the NVRs (name-version-release) of RPM packages in OS package trees and/or installations:

- An OS tree can be referenced by an url or directory containing a tree of rpm files.
- A file containing a list(s) of rpm NVRs can also be compared.
- Commands can also be used to get installed RPMs, eg:
  - `"rpm -qa"`
  - `"ssh myhost rpm -qa"`
  - `"podman run --rm myimage rpm -qa"`
- A koji tag: koji://tag@KojiHubUrl (or koji://tag@fedora)

## Usage examples

### Containers

Compare the content of two rpm based containers (new added packages in fedora:31)

```bash session
$ pkgtreediff --new "podman run --rm fedora:30 rpm -qa" "podman run --rm fedora:31 rpm -qa"
libgomp.x86_64  9.2.1-1.fc31
tss2.x86_64  1331-2.fc31
yum.noarch  4.2.9-5.fc31
```

### Package trees

Package source changes between Fedora 30 and 31 Server at GA (ignoring release bumps):

```bash session
$ pkgtreediff --ignore-release https://dl.fedoraproject.org/pub/fedora/linux/releases/{30,31}/Server/source/tree/Packages/
- 389-ds-base.src  1.4.1.2-2.fc30
- GConf2.src  3.2.6-25.fc30
- GeoIP.src  1.6.12-5.fc30
- GeoIP-GeoLite-data.src  2018.06-3.fc30
- ImageMagick.src  6.9.10.28-1.fc30
ModemManager.src: 1.10.0-1.fc30 -> 1.10.6-2.fc31
NetworkManager.src: 1.16.0-1.fc30 -> 1.20.4-1.fc31
- ORBit2.src  2.14.19-21.fc30
- OpenEXR.src  2.2.0-16.fc30
:
- compface.src  1.5.2-27.fc30
- comps-extras.src  24-5.fc30
+ conmon.src  2.0.1-1.fc31
container-selinux.src: 2.95-1.gite3ebc68.fc30 -> 2.117.0-1.gitbfde70a.fc31
- container-storage-setup.src  0.11.0-5.dev.git413b408.fc30
+ containerd.src  1.2.6-2.20190627gitd68b593.fc31
:
:
zchunk.src: 1.1.1-3.fc30 -> 1.1.2-3.fc31
zd1211-firmware.src: 1.5-4.fc30 -> 1.5-5.fc31
- zerofree.src  1.1.1-3.fc30
- zfs-fuse.src  0.7.2.2-11.fc30
- zile.src  2.4.14-3.fc30
zip.src: 3.0-24.fc30 -> 3.0-25.fc31
zlib.src: 1.2.11-15.fc30 -> 1.2.11-19.fc31
zram.src: 0.3-1.fc30 -> 0.4-1.fc31
zstd.src: 1.3.8-2.fc30 -> 1.4.2-1.fc31
- zziplib.src  0.13.69-5.fc30

Summary
Updated: 918
Added: 17
Deleted: 765
Arch changed: 0
Total packages: 1690 -> 942
```

### Hosts

Compare the packages on local and another host:

```
pkgtreediff "rpm -qa" "ssh otherhost rpm -qa"
```

Any types of sources can be compared, together with the use of flags.

### Koji

Compare koji tags using the `koji://tag@kojihub` syntax:

```
pkgtreediff koji://dist-c8-updates-build@centos koji://dist-c8_1-updates-build@centos
```

Please avoid using koji tag to compare full release as it is more efficient to
query URL or CMD trees.

## Builds

Builds are available in
[copr](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/)
for Fedora, EPEL, and OpenSuSE
([more details](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/monitor/detailed)).

## RPM version ordering
RPM version ordering is somewhat involved
<https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/>.
For example "1.2.1^rc1" < "1.2.1" (similarly for tilde).

pkgtreediff tries to implement roughly the rpmvercmp() algorithm,
but it has not been verified to behave identically.
