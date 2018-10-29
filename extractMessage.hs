-- Exercise 12
-- This exercises concerns a steganography technique used to hide a secret message inside a text file. The
-- hiding technique is to replace a letter O with a digit 0, and likewise the letter I with the digit 1. For
-- simplicity our secret message is assumed to use only the letters a, b, c and d, each of which is encoded by the
-- bit string 00, 01, 10, and 11, respectively. For example, to send the message “bad”, the bit string 010011
-- needs to be hidden in the text file. Suppose the text file contains HI HOW ARE YOU DOING? I AM
-- DOING FINE, OK! IS IT TIME TO GO? This is then changed to HI H0W ARE YOU DO1NG? I AM
-- D0ING FINE, 0K! 1S 1T TIME TO GO? Careful reading of the text file extracts the bit string 010011,
-- which is then decoded to give “bad”, the secret message. You are asked to write a function extractMessage
-- which scans the text provided, extracts the bit string, then decodes this to give the secret message. You may
-- assume that the secret message has been correctly inserted, and that the original text file included no digits
-- prior to hiding the message.

extractMessage :: String -> String
extractMessage s 
              |length s >0 = messageCode str
              |otherwise= error "There is no string"
           where
              str= messageDecrypt s

messageDecrypt :: String -> String
messageDecrypt xs= [x|x<-xs, x=='0' || x=='1']

messageCode :: String -> String
messageCode [] = []
messageCode xs
             |(take 2 xs) == "00" = 'a': messageCode (drop 2 xs)
             |(take 2 xs) == "01" = 'b': messageCode (drop 2 xs)
             |(take 2 xs) == "10" = 'c': messageCode (drop 2 xs)
             |(take 2 xs) == "11" = 'd': messageCode (drop 2 xs)

