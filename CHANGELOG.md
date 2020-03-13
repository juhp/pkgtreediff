# Changelog

## 0.4 (2020-03-13)
- support files of lists of rpms
- [experimental] support command prefixes for running `rpm -qa`
  (commands must include a space: hint use "env " for local rpm -qa)
- fix sorting of packages by rpmvercmp() style ordering (#3)
  (fixes cumulative trees like CentOS for example)
- add --timeout option for http (#1)

## 0.3 (2019-06-06)
- add --pattern for href filename globbing
- use http-directory-0.1.4 for httpDirectory basic filtering

## 0.2.1 (2019-06-06)
- better recursion handling and href filtering

## 0.2 (2019-06-05)
- print summary
- add --ignore-arch

## 0.1 (2019-06-04)
- --ignore-version option

## 0.0 (2019-05-31)
- Initially created.
- --ignore-release option
