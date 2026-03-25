
#DO NOT RUN AGAIN, IT WILL DOUBLE DATA THAT'S ALREADY IN WSJ.txt

#SET UP

import requests
from bs4 import BeautifulSoup
import nltk
import ssl
from nltk.sentiment.vader import SentimentIntensityAnalyzer
sid = SentimentIntensityAnalyzer()

Y = 'https://www.wsj.com/news/archive/2020/10/01?page=4'
WSJ_header = {"User-Agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 11_2_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.192 Safari/537.36"}

# HEADLINE FUNCTIONS FOR 1 URL PAGE

def getheadlines(WSJ_URL):

    #IMPORTING

    WSJ = requests.get(WSJ_URL,headers = WSJ_header)
    x = WSJ.content

    #CLEANING

    x = str(x)
    x = x.split('</a></h2></div> </div>')
    count = len(x)
    final = []
    for i in range(len(x)):
        final.append(x[i].split('>')[-1])
    for n in range(len(final)):
        final[n] = final[n].replace("\\xe2\\x80\\x98", "'")
        final[n] = final[n].replace("\\xe2\\x80\\x99", "'")
    final = final[:-1]
    return [final, WSJ_URL[33:43], count]

#EXPORTING

def exportheadlines(headlines, date):
    with open("WSJ.txt", "a+") as file_object:
        for i in range(len(headlines)):
            file_object.write(headlines[i])
            file_object.write("\n")
            file_object.write(date)
            file_object.write("\n")
            file_object.write(str(sid.polarity_scores(headlines[i])['compound']))
            file_object.write("\n")

#EXECUTING
          
def execute(url):
    headline_list = getheadlines(url)
    if headline_list[2] != 1:
        exportheadlines(headline_list[0],headline_list[1])
        #each url printed here is being exported to WSJ.txt
        print(url)
    return headline_list[2]

#CYCLING

start_url = 'https://www.wsj.com/news/archive/2020/10/01?page=1'

def cycle(current_url):
    date_dict = {'2020/10/32':'2020/11/01','2020/11/31':'2020/12/01','2020/12/32':'2021/01/01','2021/01/32':'2021/02/01'}
    while current_url != 'https://www.wsj.com/news/archive/2021/02/01?page=1':
        Y = execute(current_url)
        if current_url[33:43] in date_dict:
            print("Month Switch")
            current_url.replace(current_url[33:43],date_dict[current_url[33:43]])
            current_url = current_url[:33] + date_dict[current_url[33:43]] + '?page=1'
        elif Y == 1:
            print("Day Switch")
            if int(current_url[42:43]) < 9 and int(current_url[41:42]) == 0:
                   current_url = current_url[:41] + '0' + str(int(current_url[42:43])+1) + '?page=1'
            else:
                current_url = current_url[:41] + str(int(current_url[41:43])+1) + '?page=1'
            current_url = current_url[:-1] + "1"
        else:
            current_url = current_url[:-1] + str(int(current_url[-1])+1)
            
    
#print(execute(Y))
cycle(start_url)
