# -*- coding: utf-8 -*-
"""
P Christensen

1.
Scrape a list of rum label names with sugar content.
The data is then stored in a csv file.

2.
Scrape mean ratings and other metadata about rums
"""
from lxml import html
import requests
import csv
import os
from bs4 import BeautifulSoup
os.chdir("/Users/peerchristensen/Desktop/rum project")

##################### 1. SUGAR CONTENT ##################################
page = requests.get('http://rumproject.com/rumforum/viewtopic.php?t=1683&sid=b522a1521334abe1096817edfa299933')
contents = html.fromstring(page.content)
rums = contents.xpath('//span[@class="postbody"]/text()')

rums2 =[]
for rum in rums:
    rums2.append(rum.encode('ascii', 'ignore'))

with open("rumlist.csv", 'w') as rumSugar:
    wr = csv.writer(rumSugar, quoting=csv.QUOTE_ALL)
    for rum in rums2:
        wr.writerow([rum]) 

###################### 2. RATINGS ETC. ##################################

#create list of pages
urls=[]
for i in range(0,21):
        urls.append('https://www.rumratings.com/brands?letter=&order_by=number_of_ratings&page=%d' % (i))

#iterate with BeautifulSoup
rum_data=[]
rum_info=[]
rum_ratings=[]
for url in urls:
    r=requests.get(url)
    soup = BeautifulSoup(r.content)
    rum_data.append(soup.find_all("div",{"class":"rum-title"}))
    rum_info.append(soup.find_all("div",{"class":"rum-info"}))
    rum_ratings.append(soup.find_all("div",{"class":"rum-rating-icon"})) 
    
#get label names
labs=[]
for i in rum_data:
    for j in i:    
        labs+= j
labs=labs[::5]
labels=[]
for i in labs:
    labels.append(str(i).strip("\n").rstrip())
labels2=[]
for i in labels:
    labels2.append(i.encode('ascii','ignore'))

#get info
info=[]
for i in rum_info:
    for j in i:    
        info+= j
info2=[]
for i in info:
    info2.append(str(i).split("|"))

#extract info to lists
country=[]
category=[]
n_ratings=[]
price=[]
for i in info2:
    country.append(i[0].strip("\n").rstrip())
    category.append(i[1].strip(" "))
    n_ratings.append(i[2].strip("\n ratings").rstrip().lstrip())
    try:
        price.append(i[3].strip("\n $").rstrip().lstrip())
    except:
        price.append("NA")

#get ratings
rats=[]
for i in rum_ratings:
    for j in i:    
        rats+= j
ratings=[]
for i in rats:
    ratings.append(str(i).strip("\n"))

#write file
with open("rumratings.csv", 'w') as rumRating:
    wr = csv.writer(rumRating,delimiter=';', quoting=csv.QUOTE_ALL)
    wr.writerows(zip(labels2,country,category,ratings,n_ratings,price)) 




'''
rows=zip(labels2,country,ratings,n_ratings,price)
with open("rumratings2.csv", 'w') as rumRating:  
    for row in rows:
        wr = csv.writer(rumRating,delimiter=';', quoting=csv.QUOTE_ALL)
        wr.writerow(row)
'''

#Not necessary after all...
#file for extracting prices
rows=zip(labels2,price)
with open("rum_prices.csv", 'w') as rumPrices:
    for row in rows:
        wr = csv.writer(rumPrices,delimiter=',', quoting=csv.QUOTE_ALL)
        wr.writerow(row)

#iterate with better output #DOESN'T WORK!

contents2=[]
for url in urls:
    page=requests.get(url)
    con=html.fromstring(page.content)
    contents2+=con

labels=[]
for i in contents2:
    labels +=i.xpath('//div[@class="rum-title"]/text()')
labels=labels[::3]

notices = soup.find_all("div", {"class" : "rum-title"})
con = []
for a in notices:
    con+= a.contents
        
#RATINGS (UNUSED)
'''
page2 = requests.get('https://www.rumratings.com/brands?letter=&order_by=number_of_ratings&page=1')
contents2 = html.fromstring(page2.content)

#content
labels = contents2.xpath('//div[@class="rum-title"]/text()')
rating = contents2.xpath('//div[@class="rum-rating-icon"]/text()')
info = contents2.xpath('//div[@class="rum-info"]/text()')

#tidy up
#use % (modulo) to get rid of empty lines
#or...
#(for some reason, this loop had to be repeated several times)
for l in labels:
    if len(l) < 3:
        labels.remove(l)
#divide info into country, n ratings and price
'''

     
