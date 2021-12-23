{ self, inputs, ... }:

with inputs;

[
  (final: prev: {
    __dontExport = true;
    lib = prev.lib.extend (lfinal: lprev: { our = self.lib; });
  })
]

