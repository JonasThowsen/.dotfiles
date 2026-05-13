local terminal = {
  buf = nil,
  win = nil,
}

local function terminal_is_running(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then
    return false
  end

  local job_id = vim.b[buf].terminal_job_id
  if not job_id then
    return false
  end

  return vim.fn.jobwait({ job_id }, 0)[1] == -1
end

local function open_window()
  vim.cmd("botright split")
  terminal.win = vim.api.nvim_get_current_win()
end

function terminal.open()
  if terminal.win and vim.api.nvim_win_is_valid(terminal.win) then
    vim.api.nvim_set_current_win(terminal.win)
    return
  end

  open_window()

  if terminal_is_running(terminal.buf) then
    vim.api.nvim_win_set_buf(terminal.win, terminal.buf)
  else
    vim.cmd("terminal")
    terminal.buf = vim.api.nvim_get_current_buf()
    vim.bo[terminal.buf].bufhidden = "hide"
    vim.bo[terminal.buf].filetype = "terminal"
  end
end

function terminal.toggle()
  if terminal.win and vim.api.nvim_win_is_valid(terminal.win) then
    local win = terminal.win
    terminal.win = nil

    if vim.api.nvim_win_get_buf(win) == terminal.buf and #vim.api.nvim_list_wins() > 1 then
      vim.api.nvim_win_close(win, false)
    else
      vim.api.nvim_set_current_win(win)
      vim.cmd("hide enew")
    end

    return
  end

  terminal.open()
end

function terminal.kill()
  if terminal.win and vim.api.nvim_win_is_valid(terminal.win) then
    vim.api.nvim_win_close(terminal.win, true)
  end

  if terminal.buf and vim.api.nvim_buf_is_valid(terminal.buf) then
    local job_id = vim.b[terminal.buf].terminal_job_id
    if job_id then
      vim.fn.jobstop(job_id)
    end
    vim.api.nvim_buf_delete(terminal.buf, { force = true })
  end

  terminal.buf = nil
  terminal.win = nil
end

vim.keymap.set("n", "<leader>tt", terminal.toggle, { desc = "Toggle persisted terminal" })
vim.keymap.set("n", "<leader>tk", terminal.kill, { desc = "Kill persisted terminal" })

vim.api.nvim_create_user_command("PersistedTerminalToggle", terminal.toggle, {})
vim.api.nvim_create_user_command("PersistedTerminalKill", terminal.kill, {})

package.loaded.terminal = terminal
