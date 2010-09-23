" Marker insert.
nmap mi i/***koog  ***//***end***/<ESC>15<left>i
" Marker expand.
nmap me ms:exe "%!koog -io -f %:p --line " . line(".")<CR>`s
" Marker remove.
nmap mr ms:exe "%!koog -r -io -f %:p --line " . line(".")<CR>`s
" Marker all.
nmap ma ms:exe "%!koog -io -f %:p"<CR>`s
