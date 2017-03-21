if has("gui_macvim")
    macmenu &File.New\ Tab key=<nop>
    macmenu File.Print key=<nop>
    "map <D-t> :CommandT<CR>
    map <D-t> <Plug>PeepOpen
endif

