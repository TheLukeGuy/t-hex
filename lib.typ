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
  let actual = if actual == length and relative in allowed {
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

#let _left-pad-with-zeroes(unpadded, len) = {
  let unpadded-len = unpadded.len()
  if unpadded-len < len {
    let diff = len - unpadded-len
    "0" * diff
  }
  unpadded
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

#let number-format = _enum("binary", "decimal", "hex")
#let byte-repr = _enum("binary", "decimal", "hex", "ascii-text")

#let views = (
  default: (byte-repr.hex, byte-repr.ascii-text),
  default-with-binary: (byte-repr.binary, byte-repr.hex, byte-repr.ascii-text),
)

#let _default-line-number-format = number-format.decimal
#let _default-view = views.default
#let _default-bytes-per-group = 2
#let _default-groups-per-line = auto
#let _default-max-groups-per-line = none
// TODO: #let _default-group-separator-overrides-by-format = (ascii-text: none)
#let _hide-null-bytes-default = false
#let _uppercase-hex-letters-default = false
#let _default-plain-text-fallback-char = "."
#let _default-row-inset = (x: 0.3em, y: 0.25em)
#let _default-view-separator-len = 0.5em
#let _use-standard-table-default = false

#let _handle-common-displayable-args(
  data,
  hide-null-bytes,
  uppercase-hex-letters,
  plain-text-fallback-char,
) = {
  let data = if type(data) != bytes {
    // PANIC: The data may not be convertible to bytes.
    bytes(data)
  } else {
    data
  }

  _check-type("hide-null-bytes", hide-null-bytes, bool)
  _check-type("uppercase-hex-letters", uppercase-hex-letters, bool)
  _check-type("plain-text-fallback-char", plain-text-fallback-char, str)
  if plain-text-fallback-char.clusters().len() != 1 {
    panic("plain-text-fallback-char: not a single grapheme cluster")
  }

  data
}

#let displayable-group(
  data,
  repr,
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-hex-letters: _uppercase-hex-letters-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
) = {
  let data = _handle-common-displayable-args(
    data,
    hide-null-bytes,
    uppercase-hex-letters,
    plain-text-fallback-char,
  )
  for idx in range(data.len()) {
    let byte = data.at(idx)
    let show-byte = not hide-null-bytes or byte != 0
    if repr == byte-repr.binary {
      if show-byte {
        _left-pad-with-zeroes(str(byte, base: 2), 8)
      } else {
        " " * 8
      }
    } else if repr == byte-repr.decimal {
      if show-byte {
        _left-pad-with-zeroes(str(byte), 3)
      } else {
        " " * 3
      }
    } else if repr == byte-repr.hex {
      if show-byte {
        let unpadded = str(byte, base: 16)
        let unpadded = if uppercase-hex-letters {
          upper(unpadded)
        } else {
          lower(unpadded)
        }
        _left-pad-with-zeroes(unpadded, 2)
      } else {
        " " * 2
      }
    } else if repr == byte-repr.ascii-text {
      if show-byte {
        if byte >= 0x20 and byte <= 0x7e {
          str.from-unicode(byte)
        } else {
          plain-text-fallback-char
        }
      } else {
        " "
      }
    } else {
      panic("[unexpected] found unhandled byte representation")
    }
  }
}

#let displayable-data(
  data,
  view: _default-view,
  bytes-per-group: _default-bytes-per-group,
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-hex-letters: _uppercase-hex-letters-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
) = {
  let data = _handle-common-displayable-args(
    data,
    hide-null-bytes,
    uppercase-hex-letters,
    plain-text-fallback-char,
  )

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

  _check-type("bytes-per-group", bytes-per-group, int)

  let transformed-view = ()
  for group in _partition-array(data, bytes-per-group) {
    let transformed-group = ()
    for (idx, repr) in view.enumerate() {
      let displayable = displayable-group(
        group,
        repr,
        hide-null-bytes: hide-null-bytes,
        uppercase-hex-letters: uppercase-hex-letters,
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
  line-number-format,
  view-len,
  groups-per-line,
  row-inset,
  view-separator-len,
  use-standard-table,
) = {
  style(styles => {
    let view-separator = if not use-standard-table {
      let text-height = measure(raw("0"), styles).height
      let row-height = text-height + (row-inset.y * 2)
      let separator-line = box(line(length: row-height, angle: 90deg))
      if view-separator-len != 0pt {
        separator-line
        h(view-separator-len)
        separator-line
      } else {
        separator-line
      }
    } else if view-separator-len != 0pt {
      h(view-separator-len, weak: true)
    }

    let data-cell = if use-standard-table {
      groups => raw(groups.join(" "))
    } else {
      groups => stack(
        row-inset.y,
        {
          let h = h(row-inset.x)
          h
          raw(groups.join(" "))
          h
        },
        row-inset.y,
      )
    }
    let children = ()
    for line in _partition-array(displayable-data, groups-per-line) {
      let view = ((),) * view-len
      for group in line {
        for (idx, repr) in group.enumerate() {
          view.at(idx).push(repr)
        }
      }
      let view = view.map(data-cell)

      let line = view.intersperse(view-separator)
      // TODO: Add line numbers
      children += line
    }

    let columns = if view-separator != none {
      view-len * 2 - 1
    } else {
      view-len
    }
    if use-standard-table {
      let inset = if row-inset.x < row-inset.y {
        row-inset.y
      } else {
        row-inset.x
      }
      table(columns: columns, inset: inset, ..children)
    } else {
      block(
        stroke: 1pt,
        table(columns: columns, stroke: none, inset: 0pt, ..children),
      )
    }
  })
}

#let display(
  data,
  line-number-format: _default-line-number-format,
  view: _default-view,
  bytes-per-group: _default-bytes-per-group,
  groups-per-line: _default-groups-per-line,
  max-groups-per-line: _default-max-groups-per-line,
  // TODO: group-separator-overrides-by-format
  hide-null-bytes: _hide-null-bytes-default,
  uppercase-hex-letters: _uppercase-hex-letters-default,
  plain-text-fallback-char: _default-plain-text-fallback-char,
  row-inset: _default-row-inset,
  view-separator-len: _default-view-separator-len,
  use-standard-table: _use-standard-table-default,
) = {
  _check-enum-value(
    "line-number-format",
    number-format,
    line-number-format,
    optional: true,
  )

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

  let row-inset-type = _check-type("row-inset", row-inset, relative, dictionary)
  let row-inset = if row-inset-type == dictionary {
    for key in row-inset.keys() {
      if key not in ("x", "y") {
        panic(
          "row-inset: unexpected key \""
            + key
            + "\", valid keys are \"x\" and \"y\""
        )
      }
    }
    (
      x: if "x" in row-inset {
        _check-type("row-inset.x", row-inset.x, relative)
        row-inset.x
      } else {
        _default-row-inset.x
      },
      y: if "y" in row-inset {
        _check-type("row-inset.y", row-inset.y, relative)
        row-inset.y
      } else {
        _default-row-inset.y
      },
    )
  } else {
    (x: row-inset, y: row-inset)
  }

  _check-type("view-separator-len", view-separator-len, length)
  _check-type("use-standard-table", use-standard-table, bool)

  let displayable = displayable-data(
    data,
    view: view,
    bytes-per-group: bytes-per-group,
    hide-null-bytes: hide-null-bytes,
    uppercase-hex-letters: uppercase-hex-letters,
    plain-text-fallback-char: plain-text-fallback-char,
  )

  let view-len = view.len()
  if groups-per-line != auto {
    return _display(
      displayable,
      line-number-format,
      view-len,
      groups-per-line,
      row-inset,
      view-separator-len,
      use-standard-table,
    )
  }
  layout(container-size => style(styles => {
    let last = none
    let result = for groups-per-line in range(1, displayable.len() + 1) {
      let test = _display(
        displayable,
        line-number-format,
        view-len,
        groups-per-line,
        row-inset,
        view-separator-len,
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
        last
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
