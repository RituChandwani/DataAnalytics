# To execute from CLI, use the command :  python Solution.py data.txt
# 1. Program starts with getWinningWord() method which takes text fileName and ordered Dictionary
#  wordCount: ("String",{'letter',frequency}) as input and returns winningWord as output
# 2. removePunctuationExceptApostrophe() method replaces all punctuations except hyphen and apostrophes from data with space.
# 3. checkKeyExist() checks if the word is already prsent in wordCount dictionary.
# 4. getFrequencyCount() takes word as input and store it in wordCount dictionary of it has letter with highest frequency.
# 5. getHighestLetterCount() takes word as input and fetch the letter with highest repetetion in the word.
# 6. inserWordWithHighestFrequency() takes wordCount, current Word and its highest letter as input -- check if the current count
# is greater than any other letter count already present in wordCount dictionary, if yes, replace old record with the current.

from collections import defaultdict
from collections import OrderedDict
import string
import sys

def getHighestLetterCount(inputStr):
    tempStr=inputStr.lower()
    freqCount ={}
    chars = defaultdict(int)
    for char in tempStr:
        chars[char] += 1
    highest=max(chars.values())
    for k,v in chars.items():
        if v == highest:
            freqCount[k]=v
    return freqCount 


def inserWordWithHighestFrequency(wordCountDict,inputStr,freqDict):
    new_max=max(freqDict.values())
    if not wordCountDict:
        wordCount[inputStr]=freqDict
        return True        
    for key, dictValue in wordCount.items():
        curr_max=max(dictValue.values())
        if(new_max > curr_max):
            del wordCount[key]
            wordCount[inputStr]=freqDict
            return True
    return False

def checkKeyExist(wordCount,inputStr):
    if(inputStr in wordCount.keys()):
        return True
    return False


def getFrequencyCount(inputStr,wordCount):
    freqCount=getHighestLetterCount(inputStr)
    inserWordWithHighestFrequency(wordCount,inputStr,freqCount)
    return wordCount

def removePunctuationExceptApostrophe(word):
    word=word.strip()    
    table = str.maketrans({key: ' ' for key in string.punctuation if (key != "-" and key!="'")})
    new_s = word.translate(table)  
    return new_s

def getWinningWord(fileName,wordCount):
    with open(fileName, 'r') as myfile:
        data=myfile.read().replace('\n', ' ')
        data=removePunctuationExceptApostrophe(data)
    words= data.split()   
    for word in words:
        if checkKeyExist(wordCount,word) == False:
            wordCount = getFrequencyCount(word,wordCount)
    return list(wordCount.keys())[0]


wordCount=OrderedDict();
fileName=sys.argv[1]
winningWord=getWinningWord(fileName,wordCount)
print(winningWord)

  
