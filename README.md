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

Compare the content of two rpm based containers (new added packages in fedora:34)

```bash session
$ pkgtreediff --new "podman run --rm fedora:33 rpm -qa" "podman run --rm fedora:34 rpm -qa"
dejavu-sans-fonts.noarch  2.37-16.fc34
fonts-filesystem.noarch  2.0.5-5.fc34
langpacks-core-en_GB.noarch  3.0-12.fc34
langpacks-core-font-en.noarch  3.0-12.fc34
langpacks-en_GB.noarch  3.0-12.fc34
```

### Package trees

Package source changes between Fedora 30 and 31 Server at GA (ignoring release bumps):

```bash session
$ pkgtreediff --ignore-release https://dl.fedoraproject.org/pub/fedora/linux/releases/{32,33}/Server/source/tree/Packages/
ModemManager.src: 1.12.8-1.fc32 -> 1.14.2-1.fc33
NetworkManager.src: 1.22.10-1.fc32 -> 1.26.2-2.fc33
PackageKit.src: 1.1.13-2.fc32 -> 1.2.1-1.fc33
abrt.src: 2.14.0-2.fc32 -> 2.14.4-6.fc33
adwaita-icon-theme.src: 3.36.0-1.fc32 -> 3.38.0-1.fc33
alsa-lib.src: 1.2.2-2.fc32 -> 1.2.3.2-5.fc33
alsa-sof-firmware.src: 1.4.2-4.fc32 -> 1.5-2.fc33
anaconda.src: 32.24.7-1.fc32 -> 33.25.4-1.fc33
:
gobject-introspection.src: 1.64.1-1.fc32 -> 1.66.1-1.fc33
+ google-carlito-fonts.src  1.103-0.15.20130920.fc33
- google-crosextra-caladea-fonts.src  1.002-0.14.20130214.fc32
- google-crosextra-carlito-fonts.src  1.103-0.12.20130920.fc32
gpgme.src: 1.13.1-6.fc32 -> 1.14.0-2.fc33
:
:
xkeyboard-config.src: 2.29-1.fc32 -> 2.30-3.fc33
xxhash.src: 0.7.3-1.fc32 -> 0.8.0-1.fc33
yelp.src: 3.36.0-1.fc32 -> 3.38.1-1.fc33
yelp-xsl.src: 3.36.0-1.fc32 -> 3.38.1-1.fc33
- zram.src  0.4-1.fc32
zstd.src: 1.4.4-2.fc32 -> 1.4.5-5.fc33

Summary
Updated: 340
Added: 27
Deleted: 32
Arch changed: 0
Total packages: 937 -> 932
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

Please avoid using koji tag to compare full releases as it is more efficient to
query URL or CMD trees.

## Builds
pkgtreediff is packaged in Fedora.

Older builds are also available in
[copr](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/)
for Fedora, EPEL, and OpenSuSE
([more details](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/monitor/detailed)).

## RPM version ordering
RPM version ordering is somewhat involved
<https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/>.
For example "1.2.1^rc1" < "1.2.1" (similarly for tilde).

pkgtreediff tries to implement the rpmvercmp() algorithm,
but it has not been verified to behave identically.
