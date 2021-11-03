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

Compare the content of two rpm based containers versions,
filtering for new added packages:

```shellsession
$ pkgtreediff --new "podman run --rm fedora:34 rpm -qa" "podman run --rm fedora:35 rpm -qa"
libfsverity-1.4-4.fc35.x86_64
mpdecimal-2.5.1-2.fc35.x86_64
```

### Package trees

Package source changes between Fedora 33 and 34 Server at GA (ignoring release bumps):

```shellsession
$ pkgtreediff --ignore-release https://dl.fedoraproject.org/pub/fedora/linux/releases/{33,34}/Server/source/tree/Packages/
ModemManager.src: 1.14.2-1.fc33 -> 1.14.10-2.fc34
NetworkManager.src: 1.26.2-2.fc33 -> 1.30.2-1.fc34
PackageKit.src: 1.2.1-1.fc33 -> 1.2.3-1.fc34
abrt.src: 2.14.4-6.fc33 -> 2.14.5-2.fc34
:
bash.src: 5.0.17-2.fc33 -> 5.1.0-2.fc34
bash-completion.src: 2.8-9.fc33 -> 2.11-2.fc34
- bcache-tools-1.0.8-19.fc33.src
bcm283x-firmware.src: 20201008-2.63b1922.fc33 -> 20210407-1.8c7c524.fc34
bind.src: 9.11.23-1.fc33 -> 9.16.11-5.fc34
binutils.src: 2.35-10.fc33 -> 2.35.1-41.fc34
:
cockpit.src: 229-1.fc33 -> 241-1.fc34
colord.src: 1.4.4-5.fc33 -> 1.4.5-2.fc34
+ compsize-1.5-1.fc34.src
conmon.src: 2.0.21-3.fc33 -> 2.0.27-1.fc34
container-selinux.src: 2.145.0-1.fc33 -> 2.158.0-1.gite78ac4f.fc34
containernetworking-plugins.src: 0.8.7-1.fc33 -> 0.9.1-4.fc34
+ containers-common-1-13.fc34.src
- copy-jdk-configs-3.7-7.fc33.src
- createrepo_c-0.15.11-4.fc33.src
+ criu-3.15-3.fc34.src
crun.src: 0.15-5.fc33 -> 0.18-5.fc34
:
:
vim.src: 8.2.1770-1.fc33 -> 8.2.2637-1.fc34
+ vulkan-loader-1.2.162.0-2.fc34.src
wayland.src: 1.18.0-2.fc33 -> 1.19.0-1.fc34
webkit2gtk3.src: 2.30.1-1.fc33 -> 2.32.0-2.fc34
wget.src: 1.20.3-8.fc33 -> 1.21.1-2.fc34
:
zchunk.src: 1.1.5-3.fc33 -> 1.1.9-2.fc34
zstd.src: 1.4.5-5.fc33 -> 1.4.9-1.fc34

Summary
Updated: 330
Downgraded: 0
Added: 63
Deleted: 98
Arch changed: 0
Total packages: 932 -> 897
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
pkgtreediff use the [rpm-nvr](https://hackage.haskell.org/package/rpm-nvr)
library implementation of the `rpmvercmp()` algorithm,
though it has not been verified to behave identically.

RPM version ordering is somewhat involved
<https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/>.
For example "1.2.1~rc1" < "1.2.1" (greater for ^).
