
#SET UP

import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, RegexpTokenizer
tokenizer = RegexpTokenizer(r'\w+')

stop_words = stopwords.words('english')
#print(stop_words[:20])

def tokenize(headline):
    toks = tokenizer.tokenize(headline)
    toks = [t.lower() for t in toks if t.lower() not in stop_words]
    return toks

#DATA ANALYSIS

#import data
with open("WSJ.txt") as WSJ:
    cache = WSJ.readlines()
#print(len(cache)/3)

pos_headlines = []
pos_headlines_val = []
pos_words = []
neu_headlines = []
neu_headlines_val = []
neu_words = []
neg_headlines = []
neg_headlines_val = []
neg_words = []

#tokenize line, then sort data into pos/neg/neu
for i in range(int(len(cache)/3)):
    line = cache[3*i].replace("\n","")
    val = cache[3*i+2].replace("\n","")
    #print(line)
    word_list = tokenize(line)
    #print(word_list)
    if float(val) > 0.2:
        pos_headlines.append(line)
        pos_headlines_val.append(val)
        pos_words.extend(word_list)
    elif float(val) < -0.2:
        neg_headlines.append(line)
        neg_headlines_val.append(val)
        neg_words.extend(word_list)
    else:
        neu_headlines.append(line)
        neu_headlines_val.append(val)
        neu_words.extend(word_list)

#WORD VALENCE

def word_valence(word):
    total = 0
    count = 0
    for i in range(len(pos_headlines)):
        if word in tokenize(pos_headlines[i]):
            total = total + float(pos_headlines_val[i])
            count = count + 1
    for i in range(len(neg_headlines)):
        if word in tokenize(neg_headlines[i]):
            total = total + float(neg_headlines_val[i])
            count = count + 1
    for i in range(len(neu_headlines)):
        if word in tokenize(neu_headlines[i]):
            total = total + float(neu_headlines_val[i])
            count = count + 1
    total = total/(len(neu_headlines) + len(pos_headlines) + len(neg_headlines))
    return total

#RESULTS
'''
neg_freq = nltk.FreqDist(neg_words)
pos_freq = nltk.FreqDist(pos_words)
neu_freq = nltk.FreqDist(neu_words)
print(neg_freq.most_common(20))
print(pos_freq.most_common(20))
print(neu_freq.most_common(20))
print(len(neg_headlines))
print(len(pos_headlines))
print(len(neu_headlines))
'''
print(word_valence("salt"))
