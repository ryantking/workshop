-- Fennel setup
fennel = require 'fennel'
fennel.path = fennel.path .. ";/Users/ryan/.hammerspoon/?.fnl"
table.insert(package.loaders or package.searchers, fennel.searcher)
debug.traceback = fennel.traceback

-- Stackline setup
stackline = require'stackline'
stackline:init()

require 'core'
