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

#let data = read("data/dos-obj", encoding: none)
#t-hex.display(
  data,
  stroke: 1.5pt + purple,
  radius: 5pt,
  row-inset: (x: 0.45em, y: 0.375em),
)
