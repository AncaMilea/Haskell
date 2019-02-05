-- it does tidy up the input and check that the input parses to integers
fileMZip :: () -> IO ()
fileMZip () = catch (fileMZip' ()) filehandler 

fileMZip' () = do s <- readFile "lists.csv"
                   let ls = lines(s)
                   let sss = map (splitOn ",") ls
                   let iss = map (map read) sss :: [[Int]]
                   let mss = multiZipL iss
                   let oss = map (map show) mss
                   let css = intercalate "\n" (map (intercalate ",") oss) 
                   writeFile "ziplists.csv" css

filehandler :: IOException -> IO ()  
filehandler e = do let err = show (e :: IOException)
                   hPutStr stderr ("Warning: Couldn't open input/output file : " ++ err)
                   return ()