#SET UP

import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, RegexpTokenizer
tokenizer = RegexpTokenizer(r'\w+')
import matplotlib.pyplot as plt
import numpy as np
stop_words = stopwords.words('english')

def tokenize(headline):
    toks = tokenizer.tokenize(headline)
    toks = [t.lower() for t in toks if t.lower() not in stop_words]
    return toks

#DATA ANALYSIS

#import data
with open("WSJ.txt") as WSJ:
    cache = WSJ.readlines()
#print(len(cache)/3)



def analysis(word):
    vals = []
    dates = []
    month_dict = { "10":0, "11": 31, "12": 61, "01": 92}
    for i in range(int(len(cache)/3)):
        line = cache[3*i].replace("\n","")
        if word in tokenize(line):
            date = cache[3*i+1].replace("\n","")
            val = cache[3*i+2].replace("\n","")
            #print(date)
            #print(line)
            #print(val)
            day = date[-2:]
            if day[0] == "0":
                day = int(day[1])
            else:
                day = int(day)
            date = month_dict[date[5:7]] + day
            #print(date)
            vals.append(val)
            dates.append(date)
    print(len(dates))
    print(len(vals))
    return [dates,vals]

def export(dates,vals,word):
    with open("WSJ_" + word + ".txt", "a+") as file_object:
        file_object.write("Date")
        file_object.write("\t")
        file_object.write("Val")
        file_object.write("\n")
        for i in range(len(dates)):
            file_object.write(str(dates[i]))
            file_object.write("\t")
            file_object.write(str(vals[i]))
            file_object.write("\n")

def plot_word(dates,vals):
    plt.scatter(dates, vals, label= "stars", color= "green", 
                marker= "*", s=10)
      
    # x-axis label
    plt.xlabel('x - axis')
    # frequency label
    plt.ylabel('y - axis')
    plt.yticks(np.arange(-1, 1, 0.1))
    #plt.ylim(ymin=-1, ymax=1)
    # plot title
    plt.title('My scatter plot!')
    # showing legend
    plt.legend()
    # function to show the plot
    plt.show()

word = "market"
data = analysis(word)
export(data[0],data[1],word)
#plot_word(data[0],data[1])
#plot_word([-10,35,99,0,29,50],[0.01873465987,0.1,0.20,0.55,0.79,0.35])
print(data[0])
print(data[1])
