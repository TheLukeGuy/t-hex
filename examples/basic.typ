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
#import t-hex: views

#let data = (
  dos-obj: read("data/dos-obj", encoding: none),
  png: read("data/png", encoding: none),
)

#t-hex.display(data.dos-obj)
#t-hex.display(data.png, bytes-per-group: 8)
#t-hex.display(
  "Hello, world!",
  view: views.default-with-binary,
  uppercase-hex-letters: true,
)
