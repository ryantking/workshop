local compile = require("aniseed.compile")

local base = vim.fn.stdpath("config")
compile.glob("**/*.fnl", base .. "/fnl", base .. "/lua")
