#palindrome sentense
str = 'Madam, this car is a honda civic'

def palindrome_words(asentense) :
    #clean punctuations
    punc = '''!()-[]{};:'"\,<>./?@#$%^&*_~'''
    cleaned = ''
    for ele in asentense:
        if ele not in punc:
            cleaned += ele
    
    #split sentence to list
    word = cleaned.split()
    result = []
    
    for string in word:
        if len(string) < 2:  continue
        
        i = 0
        
        while (i < (len(string) - 1)/2) :
            #match upper and lowercase by ASCII value 
            if (abs(ord(string[i]) - ord(string[-i - 1])) != 32 
                and string[i] != string[-i - 1]) : break
            i += 1
        
        if i < (len(string) - 1)/2: continue
        result.append(string)
        
    return result

palindrome_words(str)


