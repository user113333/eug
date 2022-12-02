ones = [ "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine" ]
tens = [ "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety" ]
tens_cardinal = [ "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen" ]
hundreds = map (++ "Hundred") ones

msl l = sum $ map length l

one_hundred = (9 * (msl ones)) + (msl tens_cardinal) + (10 * (msl tens))

func = (one_hundred * 10) + (length "onethousand") + ((msl hundreds) * 100) + (99 * 9 * 3)

main = do
    print func
