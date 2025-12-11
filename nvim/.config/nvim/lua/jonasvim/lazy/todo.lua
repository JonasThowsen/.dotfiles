return {
  'folke/todo-comments.nvim',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local todo = require('todo-comments')

    todo.setup({
      signs = true,      -- show icons in the sign column
      sign_priority = 8, -- sign priority
      keywords = {
        FIX = {
          icon = ' ',      -- icon used for the sign
          color = 'error', -- can be a hex color, or a named color
          alt = { 'FIXME', 'BUG', 'FIXIT', 'ISSUE' },
        },
        TODO = { icon = ' ', color = 'info' },
        HACK = { icon = ' ', color = 'warning' },
        WARN = { icon = ' ', color = 'warning', alt = { 'WARNING', 'XXX' } },
        PERF = { icon = ' ', alt = { 'OPTIM', 'PERFORMANCE', 'OPTIMIZE' } },
        NOTE = { icon = ' ', color = 'hint', alt = { 'INFO' } },
        TEST = { icon = '⏲ ', color = 'test', alt = { 'TESTING', 'PASSED', 'FAILED' } },
      },
      highlight = {
        multiline = true,
        multiline_pattern = '^.',
        multiline_context = 10,
        before = '',
        keyword = 'wide',
        after = 'fg',
        -- Modify this pattern to explicitly match Python comments
        pattern = [=[#\s*<(KEYWORDS)\s*:]=], -- for Python only
        -- or use this for multiple languages including Python:
        -- pattern = [=[[//#]\s*<(KEYWORDS)\s*:]=],
        comments_only = true,
      },
    })

    -- Keymaps
    vim.keymap.set('n', ']t', function()
      todo.jump_next()
    end, { desc = 'Next todo comment' })

    vim.keymap.set('n', '[t', function()
      todo.jump_prev()
    end, { desc = 'Previous todo comment' })

    -- Search through all project todos with Telescope
    vim.keymap.set(
      'n',
      '<leader>ft',
      '<cmd>TodoFzfLua<cr>',
      { desc = 'Search through all project todos' }
    )
  end,
}
