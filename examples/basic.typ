// Copyright © 2023 Luke Chambers
//
// To the extent possible under law, the author(s) have dedicated all copyright
// and related and neighboring rights to this software to the public domain
// worldwide. This software is distributed without any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication along
// with this software. If not, see
// <http://creativecommons.org/publicdomain/zero/1.0/>.

#import "../lib.typ" as t-hex
#import t-hex: byte-repr, num-fmt

#let data = (
  dos-obj: read("data/dos-obj", encoding: none),
  pc-mbr: read("data/pc-mbr", encoding: none),
  png: read("data/png", encoding: none),
)

#t-hex.display(data.dos-obj)

#t-hex.display(
  data.pc-mbr,
  line-num-fmt: num-fmt.hex,
  line-num-padding: true,
  view: (byte-repr.octal, byte-repr.ascii-text),
  bytes-per-group: 1,
)

#t-hex.display(data.png, bytes-per-group: 4, hide-null-bytes: true)

#t-hex.display(
  "Hello, world!\n",
  line-num-fmt: none,
  view: (byte-repr.binary, byte-repr.hex, byte-repr.ascii-text),
  bytes-per-group: none,
  uppercase-digits: true,
  plain-text-fallback-char: "?",
)
