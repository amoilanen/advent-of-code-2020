;var rowBinary = "FBFBBFF".replace(/F/g, "0").replace(/B/g, "1")
;parseInt(rowBinary, 2) -- row number

;var columnBinary = "RLR".replace(/R/g, "1").replace(/L/g, "0")
;parseInt(columnBinary, 2) -- column number

; (string->number "1010" 2) -- can accept the radix parameter