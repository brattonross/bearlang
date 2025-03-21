vim.api.nvim_create_autocmd("FileType", {
	pattern = "bear",
	callback = function()
		local client = vim.lsp.start({
			name = "bearls",
			cmd = { "bear", "lsp" },
		})

		if not client then
			vim.notify("bear client could not start")
			return
		end

		vim.lsp.buf_attach_client(0, client)
	end,
})
