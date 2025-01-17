package settings

vim: {
	vimrc: enable: false
	useCtrlKeys: true

	sneak:    true
	hlsearch: true

	highlightedyank: {
		enable:   true
		duration: 300
		color:    "rgba(103, 58, 183, .4)"
	}
}

// Keybindings {{{
vim: {
	leader: "<space>"
}

#nmap: [
	{before: ["<leader>", "'"], commands: ["workbench.action.showCommands"]},
	{before: ["<leader>", "w"], commands: ["workbench.action.files.save"]},
	{before: ["<leader>", "f", "s"], commands: ["workbench.action.files.save"]},
	{before: ["<leader>", "r", "n"], commands: ["editor.action.rename"]},

	// Some workarounds
	{before: ["<leader>", "d"], after: ["\"", "_", "d"]},
	{before: ["<leader>", "D"], after: ["\"", "_", "D"]},
	{before: ["<leader>", "k"], after: ["k", "c", "c"]},
	{before: ["<leader>", "j"], after: ["j", "c", "c"]},
	{before: ["<leader>", "o"], after: ["o", "<ESC>", "c", "c"]},
	{before: ["<leader>", "O"], after: ["O", "<ESC>", "c", "c"]},

	// Splits
	{before: ["<C-H>"], after: ["<C-W>", "<C-H>"]},
	{before: ["<C-J>"], after: ["<C-W>", "<C-J>"]},
	{before: ["<C-K>"], after: ["<C-W>", "<C-K>"]},
	{before: ["<C-L>"], after: ["<C-W>", "<C-L>"]},

	// Diagnostics
	{before: ["[", "d"], commands: ["editor.action.marker.prevInFiles"]},
	{before: ["]", "d"], commands: ["editor.action.marker.nextInFiles"]},
]
// }}}

// Keybindings type checks {{{
#Remap: {
	before: [...string]
	after: [...string]
}

#Command: {
	before: [...string]
	commands: [...string]
}

#Keybinding: #Remap | #Command

vim: {
	normalModeKeyBindingsNonRecursive: [...#Keybinding] & #nmap
}
// }}}
