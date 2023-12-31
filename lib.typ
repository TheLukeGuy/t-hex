// Copyright © 2023 Luke Chambers
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

#let _enum(..values) = {
  let enum = (:)
  for (idx, value) in values.pos().enumerate() {
    enum.insert(value, idx)
  }
  enum
}

#let _check-type(name, value, ..allowed) = {
  let actual = type(value)
  let allowed = allowed.pos()
  let actual = if (
    actual not in allowed
    and actual in (length, ratio)
    and relative in allowed
  ) {
    relative
  } else {
    actual
  }

  let allowed-len = allowed.len()
  if actual not in allowed {
    let expected = if allowed-len == 1 {
      allowed.first()
    } else if allowed-len == 2 {
      allowed.first() + " or " + allowed.last()
    } else {
      allowed.join(", ", last: ", or ")
    }
    panic(name + ": expected " + expected + ", found " + actual)
  }

  if (
    allowed-len == 1
    or (allowed-len == 2 and (type(none) in allowed or type(auto) in allowed))
  ) {
    return
  }
  actual
}

#let _check-enum-value(name, enum, value, check-type: true, optional: false) = {
  if value == none {
    if optional {
      return
    }
    panic(name + ": expected enum value, found none")
  }

  if check-type {
    _check-type(name, value, int)
  }
  if value >= 0 and value < enum.len() {
    return
  }
  panic(name + ": invalid enum value")
}

#let _partition-array(unpartitioned, len) = {
  let unpartitioned-len = unpartitioned.len()
  let partitioned = ()
  for start-idx in range(unpartitioned-len, step: len) {
    let end-idx = start-idx + len
    let part = if end-idx >= unpartitioned-len {
      unpartitioned.slice(start-idx)
    } else {
      unpartitioned.slice(start-idx, end-idx)
    }
    partitioned.push(part)
  }
  partitioned
}

#let num-fmt = _enum("binary", "octal", "decimal", "hex")
#let byte-repr = _enum("binary", "octal", "decimal", "hex", "ascii-text")

#let _default-line-num-fmt = num-fmt.decimal
#let _line-num-uppercase-digits-default = true
#let _line-num-padding-default = false
#let _default-view = (byte-repr.hex, byte-repr.ascii-text)
#let _default-bytes-per-group = 2
#let _hide-null-bytes-default = false
#let _uppercase-digits-default = false
#let _default-plain-text-fallback-char = "."
#let _default-groups-per-line = auto
#let _default-max-groups-per-line = none
#let _default-stroke = 1pt + black
#let _default-radius = 0pt
#let _default-inset = (x: 0.3em, y: 0.25em)
#let _default-view-separator-len = 0.5em
#let _default-view-separator-fill = rgb(0%, 0%, 0%, 10%)
#let _default-fill = (none, rgb(0%, 0%, 0%, 5%))
#let _default-line-num-alignment = right
#let _compact-line-nums-default = true
#let _default-compact-line-num-text-fill = rgb(0%, 0%, 0%, 50%)
#let _default-group-separator-len = 0.5em
#let _default-byte-reprs-without-group-separators = (byte-repr.ascii-text,)
#let _use-standard-table-default = false

#let _handle-common-displayable-args(
  data,
  hide-null-bytes,
  uppercase-digits,
  plain-text-fallback-char,
) = {
  let data = if type(data) != bytes {
    // UNCHECKED: The data may not be convertible to bytes.
    bytes(data)
  } else {
    data
  }

  _check-type("hide-null-bytes", hide-null-bytes, bool)
  _check-type("uppercase-digits", uppercase-digits, bool)
  _check-type("plain-text-fallback-char", plain-text-fallback-char, str)
  if plain-text-fallback-char.clusters().len() != 1 {
    panic("plain-text-fallback-char: not a single grapheme cluster")
  }

  data
}

#let _num-fmt-radices = (2, 8, 10, 16)
#let _num-fmt-str-lens = (8, 3, 3, 2)

#let _str-from-int(
  value,
  fmt-or-repr,
  hide-null-values: false,
  uppercase-digits,
  pad-nums-to,
  plain-text-fallback-char: none,
) = {
  let show-value = not hide-null-values or value != 0
  if fmt-or-repr < num-fmt.len() {
    let pad-nums-to = if pad-nums-to == auto {
      _num-fmt-str-lens.at(fmt-or-repr)
    } else {
      pad-nums-to
    }
    if show-value {
      let radix = _num-fmt-radices.at(fmt-or-repr)
      let unpadded = str(value, base: radix)
      let unpadded = if radix > 10 {
        if uppercase-digits {
          upper(unpadded)
        } else {
          lower(unpadded)
        }
      } else {
        unpadded
      }

      let unpadded-len = unpadded.len()
      if unpadded-len < pad-nums-to {
        let diff = pad-nums-to - unpadded-len
        "0" * diff
      }
      unpadded
    } else {
      " " * pad-nums-to
    }
  } else if show-value {
    if value >= 0x20 and value <= 0x7e {
      str.from-unicode(value)
    } else {
      plain-text-fallback-char
    }
  } else {
    " "
  }
}

#let displayable-group(
  data,
  repr,
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-digits: _uppercase-digits-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
) = {
  let data = _handle-common-displayable-args(
    data,
    hide-null-bytes,
    uppercase-digits,
    plain-text-fallback-char,
  )
  for idx in range(data.len()) {
    let byte = data.at(idx)
    _str-from-int(
      byte,
      repr,
      hide-null-values: hide-null-bytes,
      uppercase-digits,
      auto,
      plain-text-fallback-char: plain-text-fallback-char,
    )
  }
}

#let displayable-data(
  data,
  view: _default-view,
  bytes-per-group: _default-bytes-per-group,
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-digits: _uppercase-digits-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
) = {
  let data = _handle-common-displayable-args(
    data,
    hide-null-bytes,
    uppercase-digits,
    plain-text-fallback-char,
  )
  _check-type("bytes-per-group", bytes-per-group, int, type(none))
  let bytes-per-group = if bytes-per-group != none {
    bytes-per-group
  } else {
    1
  }

  let transformed-view = ()
  for group in _partition-array(data, bytes-per-group) {
    let transformed-group = ()
    for (idx, repr) in view.enumerate() {
      let displayable = displayable-group(
        group,
        repr,
        hide-null-bytes: hide-null-bytes,
        uppercase-digits: uppercase-digits,
        plain-text-fallback-char: plain-text-fallback-char,
      )
      transformed-group.push(displayable)
    }
    transformed-view.push(transformed-group)
  }
  transformed-view
}

#let _display(
  displayable-data,
  line-num-fmt,
  line-num-uppercase-digits,
  line-num-padding,
  view,
  bytes-per-group,
  groups-per-line,
  stroke,
  radius,
  inset,
  view-separator-len,
  view-separator-fill,
  fill,
  line-num-alignment,
  compact-line-nums,
  compact-line-num-text-fill,
  group-separator-len,
  byte-reprs-without-group-separators,
  use-standard-table,
) = {
  let partitioned = _partition-array(displayable-data, groups-per-line)

  let cell(body) = if not use-standard-table {
    block(inset: inset, body)
  } else {
    body
  }
  let group-separator = h(group-separator-len)
  let data-cell(data) = {
    let group-separator = if (
      data.repr not in byte-reprs-without-group-separators
    ) {
      group-separator
    }
    let value = data.value.map(raw).join(group-separator)
    cell(value)
  }

  let bytes-per-line = if bytes-per-group != none {
    bytes-per-group * groups-per-line
  } else {
    groups-per-line
  }
  let pad-line-nums-to = if line-num-padding {
    let max-line-num = bytes-per-line * (partitioned.len() - 1)
    let radix = _num-fmt-radices.at(line-num-fmt)
    str(max-line-num, base: radix).len()
  } else {
    0
  }

  style(styles => {
    let (view-separator-line, view-separator) = if not use-standard-table {
      let text-height = measure(raw("0"), styles).height
      let row-height = text-height + (inset.y * 2)
      let separator-line = if stroke != none {
        box(line(
          length: row-height,
          angle: 90deg,
          stroke: stroke,
        ))
      }
      let separator = if view-separator-len != 0pt {
        separator-line
        h(view-separator-len)
        separator-line
      } else {
        separator-line
      }
      (separator-line, separator)
    } else {
      (none, h(view-separator-len - (inset.x * 2)))
    }

    let children = ()
    for (line-idx, line) in partitioned.enumerate() {
      let view = view.map(repr => (repr: repr, value: ()))
      for group in line {
        for (view-idx, byte) in group.enumerate() {
          view.at(view-idx).value.push(byte)
        }
      }
      let view = view.map(data-cell)

      let line = view.intersperse(view-separator)
      let line = if line-num-fmt != none {
        let line-num = bytes-per-line * line-idx
        let line-num = _str-from-int(
          line-num,
          line-num-fmt,
          line-num-uppercase-digits,
          pad-line-nums-to,
        )
        let line-prefix = if compact-line-nums {
          let cell = cell(text(fill: compact-line-num-text-fill, raw(line-num)))
          let cell = box(cell) + view-separator-line
          (align(line-num-alignment, cell),)
        } else {
          let cell = cell(raw(line-num))
          (align(line-num-alignment, cell), view-separator)
        }
        line-prefix + line
      } else {
        line
      }
      children += line
    }

    let cols = {
      let cols = view.len() * 2 - 1
      if line-num-fmt != none {
        if compact-line-nums {
          cols + 1
        } else {
          cols + 2
        }
      } else {
        cols
      }
    }

    let data-fill = fill
    let fill(col, row) = {
      let col = if compact-line-nums and line-num-fmt != none {
        if col == 0 {
          return view-separator-fill
        }
        col - 1
      } else {
        col
      }
      if calc.even(col) {
        // UNCHECKED: A user-specified function may have an invalid signature.
        data-fill(row)
      } else {
        view-separator-fill
      }
    }

    if use-standard-table {
      let inset = if inset.x < inset.y {
        inset.y
      } else {
        inset.x
      }
      table(columns: cols, fill: fill, stroke: stroke, inset: inset, ..children)
    } else {
      block(
        stroke: stroke,
        radius: radius,
        clip: true,
        table(columns: cols, fill: fill, stroke: none, inset: 0pt, ..children),
      )
    }
  })
}

#let display(
  data,
  // Data formatting options
  line-num-fmt: _default-line-num-fmt,
  line-num-uppercase-digits: _line-num-uppercase-digits-default,
  line-num-padding: _line-num-padding-default,
  view: _default-view,
  bytes-per-group: _default-bytes-per-group,
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-digits: _uppercase-digits-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
  // Sizing options
  groups-per-line: _default-groups-per-line,
  max-groups-per-line: _default-max-groups-per-line,
  // Styling options
  stroke: _default-stroke,
  radius: _default-radius,
  inset: _default-inset,
  view-separator-len: _default-view-separator-len,
  view-separator-fill: _default-view-separator-fill,
  fill: _default-fill,
  line-num-alignment: _default-line-num-alignment,
  compact-line-nums: _compact-line-nums-default,
  compact-line-num-text-fill: _default-compact-line-num-text-fill,
  group-separator-len: _default-group-separator-len,
  byte-reprs-without-group-separators: (
    _default-byte-reprs-without-group-separators
  ),
  // Advanced styling options
  use-standard-table: _use-standard-table-default,
) = {
  _check-enum-value("line-num-fmt", num-fmt, line-num-fmt, optional: true)
  _check-type("line-num-uppercase-digits", line-num-uppercase-digits, bool)
  _check-type("line-num-padding", line-num-padding, bool)

  let view-type = _check-type("view", view, int, array)
  let view = if view-type == int {
    (view,)
  } else {
    view
  }
  for (idx, repr) in view.enumerate() {
    let name = "view[" + str(idx) + "]"
    _check-enum-value(name, byte-repr, repr, check-type: false)
  }

  _check-type("groups-per-line", groups-per-line, int, type(auto))
  _check-type("max-groups-per-line", max-groups-per-line, int, type(none))
  let max-groups-per-line-present = max-groups-per-line != none
  if groups-per-line != auto {
    if groups-per-line < 1 {
      panic("groups-per-line: number must be at least 1")
    }
    if max-groups-per-line-present and max-groups-per-line < groups-per-line {
      panic("max-groups-per-line is less than groups-per-line")
    }
  }
  if max-groups-per-line-present and max-groups-per-line < 1 {
    panic("max-groups-per-line: number must be at least 1")
  }

  // TODO: Validate stroke and radius

  let inset-type = _check-type("inset", inset, relative, dictionary)
  let inset = if inset-type == dictionary {
    for key in inset.keys() {
      if key not in ("x", "y") {
        panic(
          "inset: unexpected key \""
            + key
            + "\", valid keys are \"x\" and \"y\""
        )
      }
    }
    (
      x: if "x" in inset {
        _check-type("inset.x", inset.x, relative)
        inset.x
      } else {
        _default-inset.x
      },
      y: if "y" in inset {
        _check-type("inset.y", inset.y, relative)
        inset.y
      } else {
        _default-inset.y
      },
    )
  } else {
    (x: inset, y: inset)
  }

  _check-type("view-separator-len", view-separator-len, length)

  _check-type("view-separator-fill", view-separator-fill, color, type(none))
  let fill-type = _check-type("fill", fill, color, array, function, type(none))
  let fill = if fill-type == array {
    for (idx, element) in fill.enumerate() {
      let name = "fill[" + str(idx) + "]"
      let _ = _check-type(name, element, color, type(none))
    }
    row => {
      let idx = calc.rem(row, fill.len())
      fill.at(idx)
    }
  } else if fill-type != function {
    _ => fill
  } else {
    fill
  }

  _check-type("compact-line-nums", compact-line-nums, bool)
  _check-type("compact-line-num-text-fill", compact-line-num-text-fill, color)
  _check-type("line-num-alignment", line-num-alignment, alignment)
  _check-type("group-separator-len", group-separator-len, length)

  _check-type(
    "byte-reprs-without-group-separators",
    byte-reprs-without-group-separators,
    array,
  )
  for (idx, repr) in byte-reprs-without-group-separators.enumerate() {
    let name = "byte-reprs-without-group-separators[" + str(idx) + "]"
    _check-enum-value(name, byte-repr, repr)
  }

  _check-type("use-standard-table", use-standard-table, bool)

  let displayable = displayable-data(
    data,
    view: view,
    bytes-per-group: bytes-per-group,
    hide-null-bytes: hide-null-bytes,
    uppercase-digits: uppercase-digits,
    plain-text-fallback-char: plain-text-fallback-char,
  )
  let group-separator-len = if bytes-per-group != none {
    group-separator-len
  } else {
    0pt
  }

  if groups-per-line != auto {
    return _display(
      displayable,
      line-num-fmt,
      line-num-uppercase-digits,
      line-num-padding,
      view,
      bytes-per-group,
      groups-per-line,
      stroke,
      radius,
      inset,
      view-separator-len,
      view-separator-fill,
      fill,
      line-num-alignment,
      compact-line-nums,
      compact-line-num-text-fill,
      group-separator-len,
      byte-reprs-without-group-separators,
      use-standard-table,
    )
  }
  layout(container-size => style(styles => {
    let last = none
    let result = for groups-per-line in range(1, displayable.len() + 1) {
      let test = _display(
        displayable,
        line-num-fmt,
        line-num-uppercase-digits,
        line-num-padding,
        view,
        bytes-per-group,
        groups-per-line,
        stroke,
        radius,
        inset,
        view-separator-len,
        view-separator-fill,
        fill,
        line-num-alignment,
        compact-line-nums,
        compact-line-num-text-fill,
        group-separator-len,
        byte-reprs-without-group-separators,
        use-standard-table,
      )

      let test-size = measure(test, styles)
      let fits = test-size.width <= container-size.width
      let not-reached-limit = (
        max-groups-per-line == none
        or groups-per-line != max-groups-per-line
      )

      if fits and not-reached-limit {
        last = test
      } else {
        if fits {
          test
        } else {
          last
        }
        break
      }
    }
    if result == none {
      last
    } else {
      result
    }
  }))
}

// 0x72617772727272 🦖
