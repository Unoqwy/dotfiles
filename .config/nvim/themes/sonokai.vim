function! sonokai#get_palette(style)
  if a:style ==# 'spectrum'
    let palette = {
          \ 'black':      ['#131313',   '237',  'DarkGrey'],
          \ 'bg0':        ['#191919',   '235',  'Black'],
          \ 'bg1':        ['#222222',   '236',  'DarkGrey'],
          \ 'bg2':        ['#363537',   '236',  'DarkGrey'],
          \ 'bg3':        ['#423f46',   '237',  'DarkGrey'],
          \ 'bg4':        ['#49464e',   '237',  'Grey'],
          \ 'bg_red':     ['#ff6188',   '203',  'Red'],
          \ 'diff_red':   ['#55393d',   '52',   'DarkRed'],
          \ 'bg_green':   ['#a9dc76',   '107',  'Green'],
          \ 'diff_green': ['#394634',   '22',   'DarkGreen'],
          \ 'bg_blue':    ['#78dce8',   '110',  'Blue'],
          \ 'diff_blue':  ['#354157',   '17',   'DarkBlue'],
          \ 'diff_yellow':['#4e432f',   '54',   'DarkMagenta'],
          \ 'fg':         ['#e3e1e4',   '250',  'White'],
          \ 'red':        ['#f85e84',   '203',  'Red'],
          \ 'orange':     ['#fd9353',   '215',  'Orange'],
          \ 'yellow':     ['#fce566',   '179',  'Yellow'],
          \ 'green':      ['#7bd88f',   '107',  'Green'],
          \ 'blue':       ['#5ad4e6',   '110',  'Blue'],
          \ 'purple':     ['#948ae3',   '176',  'Magenta'],
          \ 'grey':       ['#848089',   '246',  'LightGrey'],
          \ 'none':       ['NONE',      'NONE', 'NONE']
          \ }
  endif
  return palette
endfunction

