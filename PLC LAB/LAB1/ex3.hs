multiZipL :: [[a]] -> [[a]]
multiZipL [] = []
multiZipL ([] : xss) = multiZipL xss
multiZipL ((x:xs) : xss) = (x : [heaD | (heaD:_) <- xss]) : multiZipL (xs : [ taiL | (_:taiL) <- xss])