# -*- coding: utf-8 -*-
"""
Created on Wed May  2 04:57:40 2018

@author: Jane Liu
"""

import pandas as pd
import csv
import re
import time
import json
from time import gmtime, strftime, localtime
import datetime
import calendar
from datetime import date, timedelta
import uuid
from nltk.tokenize import RegexpTokenizer
from nltk.stem.porter import PorterStemmer
import codecs
import re
import nltk
from operator import itemgetter
from sklearn.feature_extraction.text import CountVectorizer
import os
import io
import numpy as np
import lda
from pandas import DataFrame
import pickle

from urllib import request
from bs4 import BeautifulSoup
import re
from time import time

url = "file:///C:/Users/Jane%20Liu/Documents/GWU%20Spring2018/MachineLearning/HW5/nysk.html"
html = request.urlopen(url).read()
#print(html[:60])

raw_HTML = BeautifulSoup(html, 'xml').get_text(strip=True)

# print(raw_HTML[0:3000])

date_time = re.split('/date', raw_HTML)
print(date_time[1])

days = []
for ele in date_time[:273]:
    # Find anything after <date>:
    mySubString=ele[ele.find("<date>")+6:-1]
    days.append(mySubString)

for ele in date_time[273:10425]:
    
    # Find anything after <date>:
    mySubString=ele[-17:]
    if mySubString == 'nation/5472582418' or mySubString == 'arketnews/archive' or mySubString == 'ment/nysk_dataset':
        pass
    else:
        days.append(mySubString)
# Convert the strings I extracted above into DateTime format.
day=[]
for i in days:
    temp = datetime.strptime(i, '%Y%m%d %H:%M:%S')
    day.append(temp)

tokenizer = RegexpTokenizer(r'\w+')

# create English stop words list
from nltk.corpus import stopwords
en_stop = stopwords.words("english")+[
        'new york', 'strausskahn','dominique', 'imf', 'chief']

# Create p_stemmer of class PorterStemmer
p_stemmer = PorterStemmer()

texts = []

# This for-loop is reading and cleaning the nysk.json data

for ib in range(1,2):
    inputf=io.open('nysk.json', 'r',  encoding='utf8',errors='ignore')
    linenum = 0;
    for line in inputf:
        whetheraboutvaccine = False
        data = json.loads(line)
        bodytexto = data['text'].encode('utf-8')
        bodytext = re.sub(r'[^\x00-\x7f]',r' ',bodytexto.decode("utf-8"))
        bodytextlower = bodytext.lower()
        timestamptext = int(data['year'])

        tokens = tokenizer.tokenize(bodytextlower)
        stopped_tokens = [i for i in tokens if not i in en_stop]
        stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]

        texts.append(stemmed_tokens)
        
    inputf.close()

# This convert the json list into a string format so that lda package can process it.

text_purestring = []
for item in texts:
    emtrys=""
    for token in item:
        emtrys=emtrys+token+" "
    text_purestring.append(emtrys)

# you all know this part now
vect = CountVectorizer(min_df=0.01, max_df=0.99,ngram_range=(1, 2))
X = vect.fit_transform(text_purestring)


# make bigrams obvious with "-"
feature_name = vect.get_feature_names()
for index,item in enumerate(feature_name):
    feature_name[index]=item.replace(" ", "-")
    
# Train the LDA model
n_topics = [2,3,4,5,6,7,8,9,10,15]
for n in n_topics:
    #print()
    model = lda.LDA(n_topics=n, n_iter=100, random_state=1)
    model.fit(X)

# Train the LDA model
model_3_topics = lda.LDA(n_topics=3, n_iter=100, random_state=1)
model_3_topics.fit(X)

topic_word = model_3_topics.topic_word_
n_top_words = 10
topic_words_frame = []
for i, topic_dist in enumerate(topic_word):
    topic_words = np.array(feature_name)[np.argsort(topic_dist)][:-(n_top_words+1):-1]
    topic_words_frame.append('Topic {}: {}'.format(i, ' '.join(topic_words)))
#print(topic_words_frame)

# The following block formats the document-topic matrix so that we can see which are the most representative topics in each document.
# The columns index is the topic id, and the row index is the document number
document_topic_matrix_in_prob = pd.DataFrame(model_3_topics.doc_topic_.T)
document_topic_matrix_in_frequency = pd.DataFrame(model_3_topics.ndz_.T)

number_of_entry=document_topic_matrix_in_frequency.shape[1]
publish_day = []
for i in range(0,number_of_entry):
    publish_day.append(day[i].day)

unique_day = np.unique(publish_day)

# Make a copy of the orginal dataframe without the dummy year.
document_topic_matrix_in_frequency_with_day = document_topic_matrix_in_frequency.copy()
# Now we have a new dataframe with a new row of made up year!
document_topic_matrix_in_frequency_with_day.loc["publish_day"] = publish_day
m =  document_topic_matrix_in_frequency_with_day.T

# Choose topic 0:
x = 0
num = len(unique_day)
# Initiate year token dictionary
token_of_topic_in_each_day={}
for i in range(num):
    token_of_topic_in_each_day[unique_day[i]]=0
    
# Add the token frequency to each year
for index, row in document_topic_matrix_in_frequency_with_day.T.iterrows():
    token_of_topic_in_each_day[row["publish_day"]] += row[x]

print (token_of_topic_in_each_day)

days_of_publish = list(token_of_topic_in_each_day.keys())

import matplotlib.pylab as plt

lists = sorted(token_of_topic_in_each_day.items()) # sorted by key, return a list of tuples

x, y = zip(*lists) # unpack a list of pairs into two tuples

plt.plot(x, y)
plt.xlabel("Day of publish")
plt.ylabel("Tokens in Topic ID 0")
plt.show()