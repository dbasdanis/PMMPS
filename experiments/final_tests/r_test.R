library("dplyr")
library(foreign)
library(stringr)


IMDB<-read.csv("movie_metadata.csv")
IMDB <- IMDB[!is.na(IMDB$imdb_score), ]
IMDB <- IMDB[!is.na(IMDB$budget), ]
IMDB <- IMDB[!is.na(IMDB$gross), ]


###################################################################################################
#                           ypologismos twn parametrwn twn actors kai twn directors
###################################################################################################

actor1 <- subset(IMDB, select = c(actor_1_name,actor_1_facebook_likes,gross,budget,imdb_score))
actor1 <- actor1 %>%
  mutate(profit = gross - budget)
actor1 <- actor1 %>% group_by(actor_1_name) %>% summarise(facebook_likes = max(actor_1_facebook_likes), total_gross = sum(as.numeric(gross)), avg_gross = mean(gross), total_profit = sum(as.numeric(profit)), avg_profit = mean(profit), avg_imdb_score = mean(imdb_score), top_imdb_score = max(imdb_score))
actor1 <- actor1[!is.na(actor1$facebook_likes), ]

actor2 <- subset(IMDB, select = c(actor_2_name,actor_2_facebook_likes,gross,budget,imdb_score))
actor2 <- actor2 %>%
  mutate(profit = gross - budget)
actor2 <- actor2 %>% group_by(actor_2_name) %>% summarise(facebook_likes = max(actor_2_facebook_likes), total_gross = sum(as.numeric(gross)), avg_gross = mean(gross), total_profit = sum(as.numeric(profit)), avg_profit = mean(profit), avg_imdb_score = mean(imdb_score), top_imdb_score = max(imdb_score))
actor2 <- actor2[!is.na(actor2$facebook_likes), ]


actor3 <- subset(IMDB, select = c(actor_3_name,actor_3_facebook_likes,gross,budget,imdb_score))
actor3 <- actor3 %>%
  mutate(profit = gross - budget)
actor3 <- actor3 %>% group_by(actor_3_name) %>% summarise(facebook_likes = max(actor_3_facebook_likes), total_gross = sum(as.numeric(gross)), avg_gross = mean(gross), total_profit = sum(as.numeric(profit)), avg_profit = mean(profit), avg_imdb_score = mean(imdb_score), top_imdb_score = max(imdb_score))
actor3 <- actor3[!is.na(actor3$facebook_likes), ]

directors <- subset(IMDB, select = c(director_name,director_facebook_likes,gross,budget,imdb_score))
directors <- directors %>%
  mutate(profit = gross - budget)
directors <- directors %>% group_by(director_name) %>% summarise(facebook_likes = max(director_facebook_likes), total_gross = sum(as.numeric(gross)), avg_gross = mean(gross), total_profit = sum(as.numeric(profit)), avg_profit = mean(profit), avg_imdb_score = mean(imdb_score), top_imdb_score = max(imdb_score))
directors <- directors[!is.na(directors$facebook_likes), ]


colnames(actor1)[1] <- "actor_name"
colnames(actor2)[1] <- "actor_name"
colnames(actor3)[1] <- "actor_name"

test = merge(actor1, actor2, all = TRUE)
actors = merge(test, actor3, all = TRUE)

rm(test)
rm(actor1)
rm(actor2)
rm(actor3)

actors <- actors %>% group_by(actor_name) %>% summarise(facebook_likes = max(facebook_likes), total_gross = sum(as.numeric(total_gross)), avg_gross = mean(avg_gross), total_profit = sum(as.numeric(total_profit)), avg_profit = mean(avg_profit), avg_imdb_score = mean(avg_imdb_score), top_imdb_score = max(top_imdb_score))


############kanonikopoiisi###########################

actors$total_gross <- actors$total_gross*10/4162540754
actors$avg_gross <- actors$avg_gross*10/658672302
actors$total_profit <- actors$total_profit*10/2039816335
actors$avg_profit <- actors$avg_profit*10/458672302

directors$total_gross <- directors$total_gross*10/4114233101
directors$avg_gross <- directors$avg_gross*10/432721657
directors$total_profit <- directors$total_profit*10/2486332231
directors$avg_profit <- directors$avg_profit*10/305024263

write.csv(actors, "actors.csv", quote=FALSE, row.names = FALSE)
write.csv(directors, "directors.csv", quote=FALSE, row.names = FALSE)


rm(IMDB)
rm(actors)
rm(directors)

###################################################################################################
#                                   ypologismos toy avg_keyword_score
###################################################################################################


keywords_score<-read.csv("keyword_score.csv")
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")


words<-read.csv("keywords_test1.csv")
words <- words[!is.na(words$imdb_score), ]
words <- words[!is.na(words$plot_keyword5), ]
words <- words[!is.na(words$plot_keyword4), ]
words <- words[!is.na(words$plot_keyword3), ]
words <- words[!is.na(words$plot_keyword2), ]
words <- words[!is.na(words$plot_keyword1), ]

test<-merge(words,keywords_score,by.x="plot_keyword1",by.y="plot_keywords")
colnames(test)[11]<-"keyword1_score"
test<-merge(test,keywords_score,by.x="plot_keyword2",by.y="plot_keywords")
colnames(test)[12]<-"keyword2_score"
test<-merge(test,keywords_score,by.x="plot_keyword3",by.y="plot_keywords")
colnames(test)[13]<-"keyword3_score"
test<-merge(test,keywords_score,by.x="plot_keyword4",by.y="plot_keywords")
colnames(test)[14]<-"keyword4_score"
test<-merge(test,keywords_score,by.x="plot_keyword5",by.y="plot_keywords")
colnames(test)[15]<-"keyword5_score"


test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[16]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[17]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[18]<-"actor3_score"
test<-merge(test,directors,by="director_name")

test$keywords_score<-(test$keyword1_score + test$keyword2_score + test$keyword3_score + test$keyword4_score + test$keyword5_score)/5
colnames(test)[20]<-"avg_keyword_score"

test <- subset(test, select = c(director_name,actor_3_name,actor_2_name,actor_1_name,imdb_score,avg_keyword_score))
write.csv(test, "test.csv", quote=FALSE, row.names = FALSE)

rm(test)
rm(words)
rm(keywords_score)
rm(actors)
rm(directors)
######################################################################################################



######################################################################################################
#                         ypologismos tou actor_score kai tou director_score
######################################################################################################




#######################################################################################################
#                                           PEIRAMA 1
#######################################################################################################

test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.2 + 
                                          avg_imdb_score*0.6 + 
                                          total_gross*0.09 + 
                                          avg_gross*0.06 + 
                                          total_profit*0.03 +
                                          avg_profit*0.02 + 
                                          nominations*0.2 + 
                                          oscars*0.1 + 
                                          ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.2 + 
                                                   avg_imdb_score*0.6 + 
                                                   total_gross*0.1125 + 
                                                   avg_gross*0.0375 + 
                                                   total_profit*0.0375 + 
                                                   avg_profit*0.0125 + 
                                                   nominations*0.2+ 
                                                   oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama1.arff")

rm(test)
rm(actors)
rm(directors)


#######################################################################################################
#                                           PEIRAMA 2
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0 + 
                              avg_imdb_score*0.8 + 
                              total_gross*0.09 + 
                              avg_gross*0.06 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0 + 
                                    avg_imdb_score*0.8 + 
                                    total_gross*0.1125 + 
                                    avg_gross*0.0375 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama2.arff")

rm(test)
rm(actors)
rm(directors)


#######################################################################################################
#                                           PEIRAMA 3
#######################################################################################################

test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.09 + 
                              avg_gross*0.06 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.1125 + 
                                    avg_gross*0.0375 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama3.arff")

rm(test)
rm(actors)
rm(directors)


#######################################################################################################
#                                           PEIRAMA 4
#######################################################################################################

test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.4 + 
                              avg_imdb_score*0.4 + 
                              total_gross*0.09 + 
                              avg_gross*0.06 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.4 + 
                                    avg_imdb_score*0.4 + 
                                    total_gross*0.1125 + 
                                    avg_gross*0.0375 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama4.arff")

rm(test)
rm(actors)
rm(directors)




#######################################################################################################
#                                           PEIRAMA 5
#######################################################################################################

test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0 + 
                              avg_gross*0.15 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0 + 
                                    avg_gross*0.15 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama5.arff")

rm(test)
rm(actors)
rm(directors)


#######################################################################################################
#                                           PEIRAMA 6
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama6.arff")

rm(test)
rm(actors)
rm(directors)



#######################################################################################################
#                                           PEIRAMA 7
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.075 + 
                              avg_gross*0.075 + 
                              total_profit*0.03 +
                              avg_profit*0.02 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.075 + 
                                    avg_gross*0.075 + 
                                    total_profit*0.0375 + 
                                    avg_profit*0.0125 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama7.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 8
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0.05 +
                              avg_profit*0 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0.05 + 
                                    avg_profit*0 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama8.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 9
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0 +
                              avg_profit*0.05 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0 + 
                                    avg_profit*0.05 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama9.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 10
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0.025 +
                              avg_profit*0.025 + 
                              nominations*0.2 + 
                              oscars*0.1 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0.025 + 
                                    avg_profit*0.025 + 
                                    nominations*0.2+ 
                                    oscars*0.1)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama10.arff")

rm(test)
rm(actors)
rm(directors)


#######################################################################################################
#                                           PEIRAMA 11
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0.05 +
                              avg_profit*0 + 
                              nominations*0.3 + 
                              oscars*0 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0.05 + 
                                    avg_profit*0 + 
                                    nominations*0.3+ 
                                    oscars*0)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama11.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 12
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.15 + 
                              avg_gross*0 + 
                              total_profit*0.05 +
                              avg_profit*0 + 
                              nominations*0 + 
                              oscars*0.3 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.15 + 
                                    avg_gross*0 + 
                                    total_profit*0.05 + 
                                    avg_profit*0 + 
                                    nominations*0+ 
                                    oscars*0.3)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama12.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 13
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0.2 + 
                              avg_gross*0. + 
                              total_profit*0 +
                              avg_profit*0 + 
                              nominations*0.3 + 
                              oscars*0 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0.2 + 
                                    avg_gross*0 + 
                                    total_profit*0 + 
                                    avg_profit*0 + 
                                    nominations*0 + 
                                    oscars*0.3)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama13.arff")

rm(test)
rm(actors)
rm(directors)

#######################################################################################################
#                                           PEIRAMA 14
#######################################################################################################


test<-read.csv("final_file_for_test.csv")
test <- subset(test, select = c(imdb_score,avg_keyword_score,actor_1_name,actor_2_name,actor_3_name,director_name))
actors<-read.csv("actors.csv")
directors<-read.csv("directors.csv")

actors <- actors %>% mutate(actor_score = top_imdb_score*0.8 + 
                              avg_imdb_score*0 + 
                              total_gross*0 + 
                              avg_gross*0 + 
                              total_profit*0.2 +
                              avg_profit*0 + 
                              nominations*0 + 
                              oscars*0.3 + 
                              ifelse(facebook_likes>=1000,0.5,0))
actors <- subset(actors, select = c(actor_name,actor_score))

directors <- directors %>% mutate(director_score = top_imdb_score*0.8 + 
                                    avg_imdb_score*0 + 
                                    total_gross*0 + 
                                    avg_gross*0 + 
                                    total_profit*0.2 + 
                                    avg_profit*0. + 
                                    nominations*0 + 
                                    oscars*0.2)
directors <- subset(directors, select = c(director_name,director_score))



test<-merge(test,actors,by.x="actor_1_name",by.y="actor_name")
colnames(test)[7]<-"actor1_score"
test<-merge(test,actors,by.x="actor_2_name",by.y="actor_name")
colnames(test)[8]<-"actor2_score"
test<-merge(test,actors,by.x="actor_3_name",by.y="actor_name")
colnames(test)[9]<-"actor3_score"
test<-merge(test,directors,by="director_name")


test <- subset(test, select = c(imdb_score,avg_keyword_score,director_score,actor1_score,actor2_score,actor3_score))
write.arff(test,file="peiramata/peirama14.arff")

rm(test)
rm(actors)
rm(directors)

