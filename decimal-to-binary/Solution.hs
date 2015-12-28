module Solution where
dec2bin :: [Char] -> [Char]
dec2bin n = _dec2bin (read n :: Int)
    where 
        _dec2bin :: Int -> [Char]
        _dec2bin 1 = "1"
        _dec2bin 0 = "0"
        _dec2bin n = _dec2bin (div n 2) ++ _dec2bin (mod n 2) 
solution = dec2bin
