local palette = {}

if opts.theme == Themes.Sonokai then
    local cfg = vim.fn['sonokai#get_configuration']()
    local colors = vim.fn['sonokai#get_palette'](cfg.style)
    if colors then
        palette['fg'] = colors.fg[1]
        palette['bg'] = colors.bg1[1]
        palette['bg_contrast'] = '#23272e'
        palette['stand_bg'] = colors.bg2[1]
        palette['red'] = colors.red[1]
        palette['orange'] = colors.orange[1]
        palette['yellow'] = colors.yellow[1]
        palette['green'] = colors.green[1]
        palette['blue'] = colors.blue[1]
        palette['purple'] = colors.purple[1]
        palette['grey'] = colors.grey[1]
    end
end

return palette

