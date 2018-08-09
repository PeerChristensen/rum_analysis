
#Rum project: data cleaning script 1
#Fall 2016
#Peer Christensen

# This script takes a csv file consisting of data from rumratings.com
# and outputs an clean version of the csv file
#------------------------------------------------

setwd("/Users/peerchristensen/Desktop/rum project")
rum_dat = read.csv("rumratings.csv", header=F,stringsAsFactors = F,sep=";")
colnames(rum_dat) <- c("Label", "Country", "Category", "Rating", "Raters", "Price")
rum_dat$Country=as.factor(rum_dat$Country)
rum_dat$Category=as.factor(rum_dat$Category)
rum_dat$Raters=as.numeric(rum_dat$Raters)
rum_dat$Label=gsub("b'","",rum_dat$Label)
rum_dat$Label=gsub("'","",rum_dat$Label)
rum_dat$Label=gsub('b"' , "" , rum_dat$Label)
rum_dat$Label=gsub('"',"",rum_dat$Label)
rum_dat$Label=gsub('Ron ',"",rum_dat$Label)
rum_dat$Label=gsub('^ ',"",rum_dat$Label)
rum_dat$Label=as.factor(rum_dat$Label)
rum_dat=rum_dat[order(Label),]
write.csv2(rum_dat,"rum_list_big.csv",sep=";")