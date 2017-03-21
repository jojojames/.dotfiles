set background=light
highlight clear
if exists("syntax on")
	syntax reset
endif
let g:colors_name="generic"
hi Normal guifg=#000000 guibg=#00ad7c
hi Comment guifg=#ea00ff guibg=NONE
hi Constant guifg=#fa0000 guibg=NONE
hi String guifg=#1a00ad guibg=NONE
hi htmlTagName guifg=#ff0000 guibg=NONE
hi Identifier guifg=#000be0 guibg=NONE
hi Statement guifg=#0040ff guibg=NONE
hi PreProc guifg=#ff80ff guibg=NONE
hi Type guifg=#004d00 guibg=NONE
hi Function guifg=#113602 guibg=NONE
hi Repeat guifg=#420342 guibg=NONE
hi Operator guifg=#ff0000 guibg=NONE
hi Error guibg=#ff0000 guifg=#ffffff
hi TODO guibg=#0011ff guifg=#ffffff
hi link character	constant
hi link number	constant
hi link boolean	constant
hi link Float		Number
hi link Conditional	Repeat
hi link Label		Statement
hi link Keyword	Statement
hi link Exception	Statement
hi link Include	PreProc
hi link Define	PreProc
hi link Macro		PreProc
hi link PreCondit	PreProc
hi link StorageClass	Type
hi link Structure	Type
hi link Typedef	Type
hi link htmlTag	Special
hi link Tag		Special
hi link SpecialChar	Special
hi link Delimiter	Special
hi link SpecialComment Special
hi link Debug		Special