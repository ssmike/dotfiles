local luasnip = require('luasnip')
local cmp = require('cmp')

-- use LSP SymbolKinds themselves as the kind labels
local kind_labels_mt = {__index = function(_, k) return k end}
local kind_labels = {}
setmetatable(kind_labels, kind_labels_mt)

local function enable_codelens(bufnr)
  pcall(vim.lsp.codelens.refresh)

  vim.api.nvim_create_autocmd({'BufWritePost', 'BufEnter', 'CursorHold'}, {
    buffer = bufnr,
    group = java_cmds,
    desc = 'refresh codelens',
    callback = function()
      pcall(vim.lsp.codelens.refresh)
    end,
  })
end

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  --enable_codelens(bufnr)
  -- Mappings
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)

  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)

  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<space>cl', '<cmd>lua vim.lsp.codelens.run()<CR>', opts)
  buf_set_keymap('n', '<space>sd', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '}d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '{d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

  -- Set some keybinds conditional on server capabilities
  if client.server_capabilities.document_formatting then
      buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.server_capabilities.document_range_formatting then
      buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  end
end

-- The nvim-cmp almost supports LSP's capabilities so You should advertise it to LSP servers..
local capabilities = vim.tbl_deep_extend('force', vim.lsp.protocol.make_client_capabilities(), require('cmp_nvim_lsp').default_capabilities())

vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

local servers = {'clangd', 'gopls', 'rust_analyzer', 'hls', 'pylsp', 'tinymist', 'zls'}

for _, lsp in ipairs(servers) do
    config = {
        on_attach = on_attach,
        capabilities = capabilities,
        codelens = { enabled = true },
    }

    if lsp == 'pylsp' or lsp == 'ty' then
        config['root_dir'] = function(bufnr, on_dir)
          local root_files = {
            'pyproject.toml',
            'setup.py',
            'setup.cfg',
            'requirements.txt',
            'Pipfile',
            '.arcadia.root'
          }
          on_dir(vim.fs.root(bufnr, root_files))
        end
    end

    vim.lsp.config(lsp, config)
    vim.lsp.enable(lsp)
end

local select_opts = {behavior = cmp.SelectBehavior.Insert}

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end
  },

  sources = {
    {name = 'path'},
    {name = 'nvim_lsp', keyword_length = 3},
    {name = 'buffer', keyword_length = 3},
    {name = 'luasnip', keyword_length = 2},
  },
  window = {
    documentation = cmp.config.window.bordered(),
    completion = cmp.config.window.bordered()
  },
  formatting = {
    fields = {'menu', 'abbr', 'kind'},
    format = function(entry, item)
      local menu_icon = {
        nvim_lsp = 'λ',
        luasnip = '⋗',
        buffer = 'Ω',
        path = '🖫',
      }

      item.menu = menu_icon[entry.source.name]
      return item
    end,
  },
  mapping = {
    ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
    ['<Down>'] = cmp.mapping.select_next_item(select_opts),

    ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
    ['<C-n>'] = cmp.mapping.select_next_item(select_opts),

    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),

    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({select = false}),

    ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
            cmp.select_next_item(select_opts)
        elseif luasnip.jumpable(1) then
            luasnip.jump(1)
        else
            fallback()
        end
    end, {'i', 's'}),

    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item(select_opts)
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, {'i', 's'}),
  },
})

g = vim.g
opt = vim.opt
cmd = vim.cmd

local lsp_progress_messages = {}
local lsp_progress_status = ''
local lsp_progress_timer = nil
local LSP_STATUS_MAX_WIDTH = 20

local function truncate_lsp_status(message)
  if vim.fn.strdisplaywidth(message) <= LSP_STATUS_MAX_WIDTH then
    return message
  end

  local suffix = '...'
  local limit = LSP_STATUS_MAX_WIDTH - vim.fn.strdisplaywidth(suffix)
  if limit <= 0 then
    return suffix
  end

  local result = ''
  for index = 1, vim.fn.strchars(message) do
    local candidate = vim.fn.strcharpart(message, 0, index)
    if vim.fn.strdisplaywidth(candidate) > limit then
      break
    end
    result = candidate
  end

  return result .. suffix
end

local function render_lsp_progress()
  local messages = {}
  for _, item in pairs(lsp_progress_messages) do
    if item.message ~= nil and item.message ~= '' then
      table.insert(messages, item.message)
    end
  end
  table.sort(messages)
  lsp_progress_status = truncate_lsp_status(table.concat(messages, ', '))
end

local function progress_message(value)
  local message = value.message or value.title or ''
  if value.percentage ~= nil and message ~= '' then
    message = string.format('%d%%: %s', value.percentage, message)
  end
  return message
end

local function clear_lsp_progress_later()
  if lsp_progress_timer ~= nil then
    lsp_progress_timer:stop()
    lsp_progress_timer:close()
  end

  lsp_progress_timer = vim.uv.new_timer()
  lsp_progress_timer:start(5000, 0, vim.schedule_wrap(function()
    lsp_progress_messages = {}
    render_lsp_progress()
    vim.cmd('redrawstatus')
  end))
end

function _G.LspStatus()
  return lsp_progress_status
end

vim.cmd([[
  function! LspStatus() abort
    return luaeval('LspStatus()')
  endfunction

  function! AirlineInit()
    call airline#parts#define_function('lsp_status', 'LspStatus')
    let g:airline_section_y = airline#section#create_right(['lsp_status', 'ffenc'])
  endfunction

  autocmd User AirlineAfterInit call AirlineInit()
]])

vim.api.nvim_create_autocmd('LspProgress', {
  callback = function(event)
    local params = event.data and event.data.params
    local value = params and params.value
    if type(params) == 'table' and type(value) == 'table' then
      local key = tostring(event.data.client_id) .. ':' .. tostring(params.token)
      if value.kind == 'end' then
        lsp_progress_messages[key] = {
          message = progress_message(value),
        }
        render_lsp_progress()
        clear_lsp_progress_later()
      else
        lsp_progress_messages[key] = {
          message = progress_message(value),
        }
        render_lsp_progress()
      end
    end
    vim.cmd('redrawstatus')
  end,
})

cmd [[
    au FileType clojure nmap <buffer> <c-]> ,gd]
    se exrc
]]

g['conjure#filetype'] = { "clojure", "fennel", "janet", "hy", "julia", "racket", "scheme", "lua", "lisp", "python", "sql" }
vim.g["conjure#mapping#doc_word"] = "gk"

vim.diagnostic.config({
    virtual_text=true
})

require("monokai-nightasty").setup({
    on_highlights = function(highlights, colors)
        highlights.LineNr = highlights.Normal
    end,
})
