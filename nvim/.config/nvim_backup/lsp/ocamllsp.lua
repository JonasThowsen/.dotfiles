return {
  cmd = { 'ocamllsp' },
  filetypes = { 'ocaml', 'ocaml.menhir', 'ocaml.interface', 'ocamllex', 'reason' },
  root_markers = {
    '.git',
    'dune-project',
    'dune-workspace',
    'esy.json',
    'package.json',
    'Makefile',
    'opam'
  },
  settings = {},
}
