isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome l = head l == last l && isPalindrome (tail $ init l)

main = do
    print $ isPalindrome [1,2,3]
    print $ isPalindrome "madamimadam"
    print $ isPalindrome [1,2,4,8,16,8,4,2,1]
