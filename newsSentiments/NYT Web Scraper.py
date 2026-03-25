
#DO NOT RUN AGAIN, IT WILL DOUBLE DATA THAT'S ALREADY IN NYT.txt

#SET UP

import requests
import nltk
import ssl
from nltk.sentiment.vader import SentimentIntensityAnalyzer
sid = SentimentIntensityAnalyzer()

#MONTHLY FUNCTIONS

def execute(date):

    #IMPORTING
    
    requestUrl = "https://api.nytimes.com/svc/archive/v1/" + date + ".json?api-key=rCf80RbOvOwZwjxOF2H0wzHbLbYFXJOB"
    requestHeaders = {
    "Accept": "application/json"
  }

    request = requests.get(requestUrl, headers=requestHeaders)
    print(request)
    STUFF = str(request.content)

    #CLEANING
    
    HEADLINES = STUFF.split('"headline":{"main":')
    for i in range(len(HEADLINES)):
        X = HEADLINES[i].split(',"')
        X = X[0]
        HEADLINES[i] = X
        HEADLINES[i] = HEADLINES[i].replace("\\xe2\\x80\\x98","'")
        HEADLINES[i] = HEADLINES[i].replace("\\xe2\\x80\\x99", "'")
        #HEADLINES[i] = HEADLINES[i].replace("\'","'")
        HEADLINES[i] = HEADLINES[i][1:-1]
    DATES = STUFF.split('"pub_date":')
    for j in range(len(DATES)):
        Y = DATES[j].split('T')
        Y = Y[0]
        DATES[j] = Y[1:]
    DATES = DATES[1:]
    HEADLINES = HEADLINES[1:]
    #print(HEADLINES)
    print(DATES[0:5])
    print(HEADLINES[5:10])
    print(len(HEADLINES))
    print(len(DATES))

    #EXPORTING
    
    with open("NYT.txt", "a+") as file_object:
        for i in range(len(DATES)):
            file_object.write(HEADLINES[i])
            file_object.write("\n")
            file_object.write(DATES[i])
            file_object.write("\n")
            file_object.write(str(sid.polarity_scores(HEADLINES[i])['compound']))
            file_object.write("\n")

#CYCLING

if __name__ == "__main__":
  execute("2020/10")
  execute("2020/11")
  execute("2020/12")
  execute("2021/1")
