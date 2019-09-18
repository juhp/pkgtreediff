# pkgtreediff

[![Hackage](https://img.shields.io/hackage/v/pkgtreediff.svg)](https://hackage.haskell.org/package/pkgtreediff)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pkgtreediff/badge/lts)](http://stackage.org/lts/package/pkgtreediff)
[![Stackage Nightly](http://stackage.org/package/pkgtreediff/badge/nightly)](http://stackage.org/nightly/package/pkgtreediff)
[![Build status](https://secure.travis-ci.org/juhp/pkgtreediff.svg)](https://travis-ci.org/juhp/pkgtreediff)

`pkgtreediff` compares RPM packages by name-version-release in OS package trees
or installations.

- OS Trees are either urls or directories containing a tree of rpm files.
- File(s) containing a list(s) of rpm NVRs can also be compared
- Command prefix(es) can be used to get installed RPMs
  - `"podman run --rm myimage"` runs `rpm -qa` in the myimage container image (or use docker if you prefer)
  - `"ssh myhost"` runs `rpm -qa` on host
  - use `"env "` for running `rpm -qa` locally (sorry, please suggest better;)

## Usage examples

### Containers

Compare the content of two rpm based containers (new packages in fedora:31)

```bash session
$ pkgtreediff "podman run --rm fedora:30" "podman run --rm fedora:31" -N
libgomp.x86_64  9.1.1-2.fc31.1
tss2.x86_64  1331-2.fc31
yum.noarch  4.2.9-1.fc31
```

### Package trees

```bash session
$ pkgtreediff --ignore-release https://dl.fedoraproject.org/pub/fedora/linux/releases/{29,30}/Workstation/source/tree/Packages/
 ImageMagick.src: 6.9.9.38-3.fc29 -> 6.9.10.28-1.fc30
 LibRaw.src: 0.19.0-6.fc29 -> 0.19.2-3.fc30
 ModemManager.src: 1.8.0-4.fc29 -> 1.10.0-1.fc30
 NetworkManager.src: 1.12.4-1.fc29 -> 1.16.0-1.fc30
 NetworkManager-openvpn.src: 1.8.6-1.fc29 -> 1.8.10-1.fc30
 NetworkManager-ssh.src: 1.2.7-5.fc29 -> 1.2.9-1.fc30
 PackageKit.src: 1.1.11-1.fc29 -> 1.1.12-5.fc30
 PyYAML.src: 4.2-0.1.b4.fc29 -> 5.1-1.fc30
 SDL2.src: 2.0.8-6.fc29 -> 2.0.9-3.fc30
 abrt.src: 2.11.0-1.fc29 -> 2.12.0-2.fc30
 adobe-source-han-code-jp-fonts.src: 2.000-6.fc29 -> 2.011-2.fc30
 adobe-source-han-sans-cn-fonts.src: 1.004-8.fc29 -> 2.000-2.fc30
 adobe-source-han-sans-jp-fonts.src: 1.004-4.fc29 -> 2.000-2.fc30
 adobe-source-han-sans-kr-fonts.src: 1.004-4.fc29 -> 2.000-2.fc30
 adobe-source-han-sans-tw-fonts.src: 1.004-9.fc29 -> 2.000-2.fc30
 adobe-source-serif-pro-fonts.src: 2.000-5.fc29 -> 2.010.1.010-1.fc30
 adwaita-icon-theme.src: 3.30.0-1.fc29 -> 3.32.0-1.fc30
- aldusleaf-crimson-text-fonts.src  0.8-0.10.20130806.fc29
- almas-mongolian-title-fonts.src  1.0-11.fc29
 alsa-lib.src: 1.1.6-3.fc29 -> 1.1.8-2.fc30
:
:
 wpa_supplicant.src: 2.6-17.fc29 -> 2.7-5.fc30
 wpan-tools.src: 0.8-3.fc29 -> 0.9-2.fc30
+ xdg-dbus-proxy.src  0.1.1-2.fc30
 xdg-desktop-portal.src: 1.0.2-1.fc29 -> 1.2.0-3.fc30
 xdg-desktop-portal-gtk.src: 1.0.2-1.fc29 -> 1.2.0-3.fc30
 xen.src: 4.11.0-7.fc29 -> 4.11.1-4.fc30
 xfsprogs.src: 4.17.0-3.fc29 -> 4.19.0-4.fc30
 xmlsec1.src: 1.2.25-5.fc29 -> 1.2.27-2.fc30
 xorg-x11-drv-ati.src: 18.0.1-2.fc29 -> 19.0.1-1.fc30
 xorg-x11-drv-libinput.src: 0.28.0-2.fc29 -> 0.28.2-1.fc30
 xorg-x11-server.src: 1.20.1-4.fc29 -> 1.20.4-3.fc30
 yelp.src: 3.30.0-1.fc29 -> 3.32.1-1.fc30
 yelp-xsl.src: 3.30.1-1.fc29 -> 3.32.1-1.fc30
+ zchunk.src  1.1.1-3.fc30
 zenity.src: 3.30.0-1.fc29 -> 3.32.0-1.fc30
 zram.src: 0.2-1.fc29 -> 0.3-1.fc30
 zstd.src: 1.3.6-1.fc29 -> 1.3.8-2.fc30

Summary
Updated: 484
Added: 70
Deleted: 53
Arch changed: 0
Total packages: 1672 -> 1689
```

### Hosts

Compare the packages on local and another host: `pkgtreediff "env " "ssh otherhost"`. Note the space after `env` to make it a command!

Any types of sources can be compared together with the use of flags.

## Builds

Builds are available in
[copr](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/)
for Fedora, EPEL, and OpenSuSE
([more details](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/monitor/detailed)).
