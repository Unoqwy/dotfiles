package settings

editor: {
	fontFamily:    "JetBrainsMono Nerd Font Mono"
	fontLigatures: true
	fontSize:      15
	lineNumbers:   "relative"
	codeLens:      false
	formatOnSave:  true
	wordWrap:      "off"
	minimap: enabled: false
	suggestSelection: "first"
}

window: {
	menuBarVisibility: "toggle"
}

workbench: {
	colorTheme: "Gruvbox Dark Hard"
	iconTheme:  "eq-material-theme-icons-palenight"
}

files: {
	insertFinalNewline: true
	exclude: {
		"**/.classpath":   true
		"**/.project":     true
		"**/.settings":    true
		"**/.factorypath": true
	}
}
