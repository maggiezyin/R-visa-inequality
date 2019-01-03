library(tidyverse)
library(lubridate)
library(plotly)
library(expss)
library(maps)
library(dplyr)

country_visas<-read.csv("country-visas.csv")
visa_categories<-read.csv("visa-categories.csv")
visa_inequality <- country_visas
country_visas_USA<-read.csv("country-visas-USA.csv")
visa_inequality_head <-read.csv("country-visas-USA.csv", header = FALSE)





# fill visa_inequality table with calculated 
# inequality scores for each country 
for(i in 1:nrow(country_visas)){
  for(j in 4:ncol(country_visas)){
    categoryA <- country_visas[i,j]
    categoryB <- country_visas[j-3, i+3]
    if(!is.null(categoryA)){
      score <- categoryB - categoryA
      visa_inequality[i,j] <- score
      visa_inequality[j-3, i+3] <- score*(-1)
    }
  }
}


vi2 <- visa_inequality
#vi2 = subset(vi2, select = -c(Mean) )


#create column of means of inequality scores 
#this will help with ranking countries
country_means_row<- rowMeans(visa_inequality[,4:ncol(visa_inequality)], na.rm=TRUE)
country_means <-  colMeans(visa_inequality[,4:ncol(visa_inequality)], na.rm=TRUE)
scores_by_country = data.frame(country= visa_inequality$country, code = visa_inequality$code,continent = visa_inequality$continent, mean =country_means_row)


#group inequality means by continent
#this will help with ranking by continent 
scores_by_continent <- summarise(group_by(scores_by_country, continent), mean=mean(mean),count=n())


#collapse table
vi2 <- gather(vi2, key = "countryTo", value = "score", AFG:ZWE)
cv2 <- gather(country_visas, key = "countryTo", value = "category", AFG:ZWE)



#assign visa requirement categories
cv2 <- mutate(cv2, category=ifelse(cv2$category==0, "Avisa not required", 
                                ifelse(cv2$category==1, "Bvisa on arrival", 
                                       ifelse(cv2$category==2, "CETA required",  
                                              ifelse(cv2$category==3, "DeVisa required", 
                                                     ifelse(cv2$category==4, "Evisa required",  
                                                            ifelse(cv2$category==5, "Fadmission refused", 
                                                                    cv2$category)))))))

#assign score names
vi2 <- mutate(vi2, score=ifelse(vi2$score==-5, "disadvantage5", 
          ifelse(vi2$score==-4, "disadvantage4", 
                ifelse(vi2$score==-3, "disadvantage3", 
                      ifelse(vi2$score==-2, "disadvantage2", 
                            ifelse(vi2$score==-1, "disadvantage1", 
                                  ifelse(vi2$score==0, "bneutral", 
                                        ifelse(vi2$score==1, "advantageE1", 
                                              ifelse(vi2$score==2, "advantageD2", 
                                                    ifelse(vi2$score==3, "advantageC3", 
                                                          ifelse(vi2$score==4, "advantageB4",
                                                                ifelse(vi2$score==5, "advantageA5", 
                                                                             vi2$score))))))))))))


#sort scores alphabetically
inequality1 <- vi2[order(vi2$score),]
travelto1 <- cv2[order(cv2$code),]
travelfrom1 <- cv2[order(cv2$countryTo),]

colnames(travelfrom1)[colnames(travelfrom1)=="code"] <- "c2"
colnames(travelfrom1)[colnames(travelfrom1)=="countryTo"] <- "code"


scores_by_country <- scores_by_country[order(-scores_by_country$mean),]
scores_by_continent <- scores_by_continent[order(-scores_by_continent$mean),]

row.names(scores_by_country) <- NULL
row.names(scores_by_continent) <- NULL

scores_by_country$rank <- row.names(scores_by_country)
scores_by_continent$rank <- row.names(scores_by_continent)

#reset row indexes
row.names(inequality1) <- NULL
row.names(travelto1) <- NULL
# 
# #subset the first country
# inequality1a <- inequality1[1:388,]
# travelto1a <- travelto1[1:194,]


inequality1 <- full_join(inequality1, scores_by_country, by="country")
travelto1 <- full_join(travelto1, scores_by_country, by="country")
travelfrom1 <- full_join(travelfrom1, scores_by_country, by="code")


inequality1 <- mutate(inequality1, display_name = paste(inequality1$rank, inequality1$country, inequality1$continent.x, sep = "  |  "))
travelto1 <- mutate(travelto1, display_name = paste(travelto1$rank, travelto1$country, travelto1$continent.x, sep = "  |  "))
travelfrom1 <- mutate(travelfrom1, display_name = paste(travelfrom1$rank, travelfrom1$country.y, travelfrom1$continent.y, sep = "  |  "))


inequality1 <-mutate(inequality1, display_name = as.factor(display_name))
travelto1 <-mutate(travelto1, display_name = as.factor(display_name))
travelfrom1 <-mutate(travelfrom1, display_name = as.factor(display_name))


inequality1 <- inequality1[order(as.numeric(inequality1$rank)),]
travelto1 <- travelto1[order(as.numeric(travelto1$rank)),]
travelfrom1 <- travelfrom1[order(as.numeric(travelfrom1$rank)),]


row.names(inequality1) <- NULL
row.names(travelto1) <- NULL
row.names(travelfrom1) <- NULL





#inequality1 <- mutate(inequality1, display_name = fct_reorder(display_name,as.numeric(as.factor(continent.x))))
#levels(inequality1$display_name)


# stacked bar for each country showing frequency of each inequality score
# HOW DO WE ORDER THIS BY INEQUALITY RANKING using scores_by_country (USA should be the first bar)
g2 <- ggplot(inequality1)+
  geom_bar(aes(x=display_name,fill=score),position="fill", width = 0.7, na.rm=TRUE)+
  scale_fill_brewer(palette="RdBu" , direction= -1, labels=c("+++++", "++++", "+++","++","+","neutral", "-","--","---", "----","-----" ),name="Inequality Score") +coord_flip() +
  labs(x="code",y="Afghanistan",
       title="")+ #theme_classic()
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.text.y = element_text(hjust = 0),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

g2




g3 <- ggplot(inequality1)+
  geom_bar(aes(x=display_name,fill=score),position="fill", width = 0.65, na.rm=TRUE)+
  scale_fill_brewer(palette="RdBu" , direction= -1, labels=c("+++++", "++++", "+++","++","+","neutral", "-","--","---", "----","-----" ),name="Inequality Score") +coord_flip() +
  labs(x="code",y="Afghanistan",
       title="")+ #theme_classic()
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

g3




# stacked bar for each country showing visa requirements for traveling TO each country
# we need to order these bars by ranking as well
g1 <- ggplot(travelfrom1)+
  geom_bar(aes(x=display_name, fill=category),position="fill", width = 0.7,na.rm=TRUE)+
  scale_fill_brewer(palette="RdBu", direction = -1,
                    name="Visa Category")+ coord_flip() +
  labs(x="h",y="a",
       title="")+ #theme_classic()
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

g1


#MAPS

world <- map_data("world")

names(world)[5]<-"country"
world[6] <- NULL
world$country <- as.factor(world$country) #synching capitalization of two data sets

german_to <- country_visas_USA[c(1:3, 65)] #sorting data to isolate difficulty for germans to enter other countries

world_german_to <- left_join(german_to,world,by='country')

german_from <-country_visas_USA[62,]
german_from2 <-visa_inequality_head[c(1,63),]
ccc <-german_to
ccc$DEU=NULL
cccLL <- left_join(ccc,world,by='country')
gf=german_from2[1:2,4:197]
gf=t(gf)
names(cccLL)[2]<-"code"
gf.frame=data.frame(gf)
names(gf.frame)[1]<-"code"
names(gf.frame)[2]<-"score"
world_german_from <- left_join(gf.frame,cccLL,by='code')#joining map data to 


#Map 2-WHERE CAN GERMANS TRAVEL 2

m1<-ggplot()+geom_polygon(data=world_german_from,aes(x=long,y=lat,group = group, fill=factor(score)))+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))

#g1<-g1+theme(plot.background = element_rect(fill = 'slateblue4'),legend.position = "bottom",plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="slateblue4"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,col="slateblue4",fill="slateblue4"))

m1<-m1+scale_fill_gradient2()
m1 <- m1 + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))

cols<-c("0"="skyblue2","1"="gold2","2"="darkorange2","3"="firebrick1","4"="firebrick2","5"="black","NA"="grey")

m1<-m1+scale_fill_manual(values = cols,labels=c("Visas not\nrequired","Visa On \n arrival","ETA \n required","eVisa \n required","Visa \n required","Admission \n refused"))+labs(x="",y="",title=NULL)


m1

#Map 3-WHO CAN TRAVEL TO GERMANY

m2<-ggplot()+geom_polygon(data=world_german_to,aes(x=long,y=lat,group = group, fill=factor(DEU)))+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))

m2<-m2+scale_fill_gradient2()
#g2<-g2+theme(plot.background = element_rect(fill = 'slateblue4'),legend.position = "bottom",plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="slateblue4"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,col="slateblue4",fill="slateblue4"))
m2 <- m2 + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))


cols<-c("0"="skyblue2","1"="gold2","2"="darkorange2","3"="firebrick1","4"="firebrick2","5"="black","NA"="grey")

m2<-m2+scale_fill_manual(values = cols,labels=c("Visas not\nrequired","Visa On \n arrival","ETA \n required","eVisa \n required","Visa \n required","Admission \n refused"))+labs(x="",y="",title=NULL)

m2

#Map 4-GERMAN INEQUALITY

comp <- left_join(gf.frame,world_german_to,by='code')#joining map data to 
names(comp)[2]<-"from"
names(comp)[5]<-"to"
comp$from=as.character(comp$from)
comp$from=as.numeric(comp$from)
comp$to=as.character(comp$to)
comp$to=as.numeric(comp$to)
comp$diff<-comp$from-comp$to
comp$neg=comp$diff*(-1)

m3<-ggplot()+geom_polygon(data=comp,aes(x=long,y=lat,group = group, fill=factor(neg)))+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))

m3<-m3+theme(plot.background = element_rect(fill = 'slateblue4'),legend.position = "bottom",plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="slateblue4"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,col="slateblue4",fill="slateblue4"))

#g3<-g3+scale_fill_gradient2()

m3 <- m3 + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))

m3

#Map 1,5- VISA INEQUALITY

inequlityLL <- left_join(vi2,cccLL,by='code')#joining map data to 
counts=modify(visa_inequality, {
  neg5=count_row_if(-5, visa_inequality)
  neg4=count_row_if(-4, visa_inequality)
  neg3=count_row_if(-3, visa_inequality)
  neg2=count_row_if(-2, visa_inequality)
  neg1=count_row_if(-1, visa_inequality)
  nuetral=count_row_if(0, visa_inequality)
  p5=count_row_if(5, visa_inequality)
  p4=count_row_if(4, visa_inequality)
  p3=count_row_if(3, visa_inequality)
  p2=count_row_if(2, visa_inequality)
  p1=count_row_if(1, visa_inequality)
})

counts2=select(counts,-c(4:197))
counts2$dis=counts2$neg5+counts2$neg4+counts2$neg3+counts2$neg2+counts2$neg1
counts2$adv=counts2$p5+counts2$p4+counts2$p3+counts2$p2+counts2$p1
counts2$score=counts2$adv-counts2$dis
counts_sum<-counts2[order(counts2$score),]
counts_sum<-counts_sum[seq(dim(counts_sum)[1],1),]
counts_sum$rank<-seq.int(nrow(counts_sum))
counts_sum$mean<-0
counts_sum$median<-21
world_ineq <- left_join(counts_sum,cccLL,by='code')#joining map data to 
world_ineq=select(world_ineq, -c(8:9))

#ATTEMPT AT INTERACTIVITY
world_ineq$hover <-with(world_ineq, paste(country.x, '<br>', dis, "Disadvantages", "<br>", adv, "Advantages", "<br>", "#", rank))

hover_map=with(world_ineq, paste(country.x, '<br>', dis, "Disadvantages", "<br>", adv, "Advantages", "<br>", "#", rank))

m4<-ggplot()+geom_polygon(data=world_ineq,aes(x=long,y=lat,group = group, fill=score))+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))

m4<-m4+scale_fill_gradient2()
#g3<-g3+theme(plot.background = element_rect(fill = 'slateblue4'),legend.position = "bottom",plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="slateblue4"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,col="slateblue4",fill="slateblue4"))
m4 <- m4 + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))


#ATTEMPT AT INTERACTIVITY
m4<-m4 %>% add_trace(z=~country,text=~hover)

m4


m5<-ggplot(data=world_german_to)+geom_polygon(aes(x=long,y=lat,group = group, fill=factor(DEU),color=country=="Germany",size=country=="Germany"))+scale_color_manual(values=c("white","black"),guide=FALSE)+scale_size_manual(values=c(.5,1.2),guide=FALSE)+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))

m5<-m5+theme(plot.background = element_rect(fill = 'white'),legend.position = "bottom",plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="white"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,fill="white"))

m5 <- m5 + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))

cols<-c("0"="skyblue2","1"="gold2","2"="darkorange2","3"="lightcoral","4"="firebrick2","5"="black")

m5<-m5+scale_fill_manual(values = cols,labels=c("0"="Visas not\nrequired","1"="Visa On \n arrival","2"="ETA \n required","3"="eVisa \n required","4"="Visa\n required","5"="Admission \n refused"),na.value="grey")+labs(x="",y="",title=NULL)

m5


# world_german_to$outline=NULL
# if(world_german_to$country=="Germany"){
#   world_german_to$outline=1
# } else{
#   world_german_to$outline=0
# }

world_german_from=modify(world_german_from, {
  outline=count_row_if("Germany", world_german_from)
})
world_german_from$score=as.character(world_german_from$score)
world_german_from$score=as.numeric(world_german_from$score)
world_german_from$score[31446] = 5
world_german_from$score=as.character(world_german_from$score)
world_german_from$score=as.factor(world_german_from$score)

world_german_from <- mutate(world_german_from, disp = paste(world_german_from$score, world_german_from$outline, sep = "."))


visa.labels=expression(c("Visas not\nrequired","Visa On \n arrival","ETA \n required","eVisa \n required","Visa \n required","Admission \n refused"))

#world_german_from$disp <- as.character(world_german_from$disp)
#world_german_from$disp[world_german_from$disp == "0.0"] <- "b"

world_german_from$disp=as.character(world_german_from$disp)
world_german_from$disp <- ifelse(world_german_from$disp=="0.0", "Visas not\nrequired", 
                                 ifelse(world_german_from$disp=="1.0", "Visa On \n arrival",
                                        ifelse(world_german_from$disp=="2.0","ETA \n required",
                                               ifelse(world_german_from$disp=="3.0","eVisa \n required",
                                                      ifelse(world_german_from$disp=="4.0","Visa \n required",
                                                             ifelse(world_german_from$disp=="5.0", "Admission \n refused",
                                                                    ifelse(world_german_from$disp=="NA.1", NA, NA)))))))
    

world_german_from$disp<-as.factor(world_german_from$disp)

world_german_to=modify(world_german_to, {
  outline=count_row_if("Germany", world_german_to)
})




world_german_to$DEU=as.character(world_german_to$DEU)
world_german_to$DEU=as.numeric(world_german_to$DEU)
world_german_to$DEU[31446] = 5
world_german_to$DEU[31445] = 1
world_german_to$DEU[10000] = 2
world_german_to$DEU[10001] = 3
world_german_to$DEU=as.character(world_german_to$DEU)
world_german_to$score=as.factor(world_german_to$DEU)

world_german_to <- mutate(world_german_to, disp = paste(world_german_to$DEU, world_german_to$outline, sep = "."))
world_german_to$disp<-as.factor(world_german_to$disp)
world_german_to$disp=as.character(world_german_to$disp)
world_german_to$disp <- ifelse(world_german_to$disp =="0.0", "Visas not\nrequired", 
                                 ifelse(world_german_to$disp =="1.0", "Visa On \n arrival",
                                        ifelse(world_german_to$disp =="2.0","ETA \n required",
                                               ifelse(world_german_to$disp =="3.0","eVisa \n required",
                                                      ifelse(world_german_to$disp=="4.0","Visa \n required",
                                                             ifelse(world_german_to$disp=="5.0", "Admission \n refused",
                                                                    ifelse(world_german_to$disp=="NA.1", NA, NA)))))))

world_german_to$disp<-as.factor(world_german_to$disp)
