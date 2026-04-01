vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    local data = ev.data
    if not data or (data.kind ~= 'install' and data.kind ~= 'update') then
      return
    end

    local spec = data.spec
    local path = data.path
    if spec.name ~= 'fff.nvim' or not path then
      return
    end

    local result = vim.system(
      { 'nix', 'run', '.#release' },
      { cwd = path }
    ):wait()

    if result.code == 0 then
      vim.notify('fff.nvim built with nix', vim.log.levels.INFO)
      return
    end

    error('fff.nvim nix build failed:\n' .. (result.stderr or 'unknown error'))
  end,
})

vim.pack.add({ 'https://github.com/dmtrKovalenko/fff.nvim' })

vim.g.fff = {
  lazy_sync = true,
    prompt = 'λ ',
    layout = {
      height = 0.9,
      width = 0.9,
    }
}

vim.keymap.set(
  'n',
  '<leader>f',
  function() require('fff').find_files() end,
  { desc = 'FFFind files' }
)

vim.keymap.set(
  'n',
  '<leader>s',
  function() require('fff').live_grep({
	  grep = {
		  modes = {'fuzzy', 'regex', 'plain'}
	  }
  }) end,
  { desc = 'FFFind files' }
)
