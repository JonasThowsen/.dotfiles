local languages = {
  'vimdoc',
  'elixir',
  'javascript',
  'typescript',
  'c',
  'lua',
  'rust',
  'jsdoc',
  'bash',
  'html',
  'css',
  'json',
  'markdown',
  'markdown_inline',
  'templ',
  'python',
  'r',
  'rnoweb',
  'yaml',
}

vim.pack.add({
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter' },
})

require('nvim-treesitter').setup({
  install_dir = vim.fn.stdpath('data') .. '/site',
})

if vim.fn.executable('tree-sitter') == 1 then
  require('nvim-treesitter').install(languages)
else
  vim.notify_once(
    'nvim-treesitter requires tree-sitter-cli in PATH to install parsers.',
    vim.log.levels.WARN,
    { title = 'Treesitter' }
  )
end

-- highlighting
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    unpack(languages),
  },
  callback = function(args)
    if vim.bo[args.buf].filetype == 'html' then
      return
    end

    local max_filesize = 100 * 1024
    local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(args.buf))
    if ok and stats and stats.size > max_filesize then
      vim.notify(
        'File larger than 100KB treesitter disabled for performance',
        vim.log.levels.WARN,
        { title = 'Treesitter' }
      )
      return
    end

    vim.treesitter.start(args.buf)

    if vim.bo[args.buf].filetype == 'markdown' then
      vim.bo[args.buf].syntax = 'on'
    end
  end,
})

-- indentation
vim.api.nvim_create_autocmd('FileType', {
  pattern = {
    unpack(languages),
  },
  callback = function(args)
    vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
