return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  opts = {
    formatters_by_ft = {
      javascript = { 'prettier' },
      typescript = { 'prettier' },
      javascriptreact = { 'prettier' },
      typescriptreact = { 'prettier' },
      json = { 'prettier' },
      jsonc = { 'prettier' },
      html = { 'prettier' },
      css = { 'prettier' },
      scss = { 'prettier' },
      markdown = { 'prettier' },
      yaml = { 'prettier' },
      astro = { 'prettier' },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_fallback = true,
    },
    formatters = {
      prettier = {
        -- Only run prettier if config file exists
        condition = function(ctx)
          return vim.fs.find({
            '.prettierrc',
            '.prettierrc.json',
            '.prettierrc.yml',
            '.prettierrc.yaml',
            '.prettierrc.js',
            '.prettierrc.cjs',
            '.prettierrc.mjs',
            'prettier.config.js',
            'prettier.config.cjs',
            'prettier.config.mjs',
          }, { path = ctx.filename, upward = true })[1]
        end,
      },
    },
  },
}
