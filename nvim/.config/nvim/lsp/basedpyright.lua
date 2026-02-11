return {
  cmd = { 'uv', 'run', 'basedpyright-langserver', '--stdio' },
  filetypes = { 'python' },
  root_markers = {
    'pyproject.toml', 'setup.py', 'setup.cfg', 'requirements.txt', 'pyrightconfig.json', '.git',
  },
  settings = {
    basedpyright = {
      analysis = {
        typeCheckingMode = 'strict',
      },
    },
  },
}
