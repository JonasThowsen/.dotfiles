return {
  cmd = { 'uv', 'run', 'ruff', 'server' },
  filetypes = { 'python' },
  root_markers = {
    'pyproject.toml', 'ruff.toml', '.ruff.toml', 'requirements.txt', '.git',
  },
  on_attach = function(client)
    -- Disable hover in favor of basedpyright
    client.server_capabilities.hoverProvider = false
  end,
  settings = {},
}
