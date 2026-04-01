return {
  cmd = { 'astro-ls', '--stdio' },
  filetypes = { 'astro' },
  root_markers = {
    'astro.config.mjs', '.git',
  },
  init_options = {
    typescript = {
      tsdk = vim.fn.system("dirname $(dirname $(which tsc))"):gsub("\n", "") .. "/lib/node_modules/typescript/lib"
    }
  },
  settings = {
  },
}
