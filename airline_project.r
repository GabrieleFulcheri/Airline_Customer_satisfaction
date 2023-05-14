install.packages('tidyverse')
install.packages('readr')
install.packages('scales')
install.packages('psych')
install.packages('polycor')
install.packages('xlsx')
install.packages('writexl')
install.packages('effectsize')
install.packages('rstatix')
install.packages('nFactors')
install.packages('factoextra')
install.packages('prettydoc')
install.packages('kableExtra')


library(nFactors)
library(kableExtra)
library(prettydoc)
library(writexl)
library(tidyverse)
library(readr)
library(scales)
library(psych)
library(open)
library(effectsize)
library(rstatix)
library(factoextra)

train <- read.csv('train.csv',header=TRUE, sep=',')
View(train)

##EDA and data cleaning

summary(train)  ##pointed us 310 na values in the arrival delay column

train %>% filter(Age < 18) %>% View()

Na_values <- train %>% filter(is.na(Arrival.Delay.in.Minutes))%>% View() 
train %>% duplicated()%>% sum() ##no duplicated values found

##EDA


train %>% count(Customer.Type)
ggplot(data = train, aes(x=Customer.Type))+ geom_bar(fill='orange', stat = 'count')+
  labs(title = "Customers' loyalty distribution", y = 'Number of customers')+
  theme_minimal()


ggplot(data = train, aes(x=Age, fill = Gender))+ geom_bar(stat = 'count', position = 'stack')+
  labs(title = 'Age distribution of customers', y = 'Number of customers')+
  geom_text(stat= 'count', aes(label=..count..),size = 2.4,vjust = - 0.5, position = 'stack')+
  theme_light()

train %>%distinct(Type.of.Travel)
ggplot(data = train, aes(x=Type.of.Travel))+ geom_bar(fill='orange', stat = 'count')+
  labs(title = 'Types of travels', y = 'Number of customers')+
  theme_minimal()

train %>%distinct(Class)
ggplot(data = train, aes(x=Class))+ geom_bar(fill = "orange", stat = 'count')+
  labs(title = "Customers'Class", y = 'Number of customers')+
  theme_minimal()

train %>%distinct(satisfaction)
train %>% count(satisfaction)
ggplot(data = train, aes(x=satisfaction, fill = Customer.Type))+ geom_bar(stat = 'count')+
  facet_wrap(~ Class)+
  labs(title = "Customers's satisfaction", y = 'Number of customers')+
  geom_text(stat= 'count', aes(label=..count..),size = 2.4,vjust = - 0.5, position = 'dodge')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

                    
train_cleaned %>% group_by(Class) %>% summarise(LoyalCustomer= sum(Customer.Type == 'Loyal Customer'),
                                         DisloyalCustomer = sum(Customer.Type == 'disloyal Customer'),
                                        SatisfiedCustomer = percent(sum(satisfaction == "satisfied")/ (LoyalCustomer + DisloyalCustomer)),
                                         Personal_travel = sum(Type.of.Travel == 'Personal Travel'),
                                         Business_travel = sum(Type.of.Travel == 'Business travel')) %>% View()


train_cleaned <- train %>% na.omit(Arrival.Delay.in.Minutes)

##Contingency tables

con1 <- table(train_cleaned$Class, train_cleaned$Customer.Type)  #Business are loyal, problem is eco and ecoplus
chisq.test(con1)
cramer_v(con1)   ##0.1234206

con2 <- table(train_cleaned$Customer.Type, train_cleaned$satisfaction)  ##loyal customers are not satisfied: no strong correlation between loyalty and satisfaction
chisq.test(con2)
cramer_v(con2)  ##0.1875332

con3 <- table(train_cleaned$Type.of.Travel, train_cleaned$satisfaction) ##58,26% of Business travelers are satisfied, while 89,84% of Personal are not satisfied
chisq.test(con3)
cramer_v(con3)  ##0.4489738

con4 <- table(train_cleaned$Class, train_cleaned$satisfaction)  ##eco and eco plus are not satisfied
chisq.test(con4)
cramer_v(con4)   ## 0.504839


##Averages


Diff_means <- train_cleaned %>% group_by(Type.of.Travel, satisfaction) %>%
  summarise(Inflight.wifi.service = mean(Inflight.wifi.service),
            Gate.location = mean(Gate.location),
            Departure.Arrival.time.convenient = mean(Departure.Arrival.time.convenient),
            Ease.of.Online.booking = mean(Ease.of.Online.booking),
            Food.and.drink = mean(Food.and.drink),
            Online.boarding = mean(Online.boarding),
            Seat.comfort = mean(Seat.comfort),
            Inflight.entertainment = mean(Inflight.entertainment),
            On.board.service = mean(On.board.service),
            Leg.room.service = mean(Leg.room.service),
            Baggage.handling = mean(Baggage.handling),
            Checkin.service = mean(Checkin.service),
            Inflight.service = mean(Inflight.service), 
            Cleanliness = mean(Cleanliness))

write_xlsx(Diff_means, path = "Diff_means.xlsx")

calc_diff_business <- function(x) {
  return(abs(x[1] - x[2]))
}

calc_diff_personal <- function(x) {
  return(abs(x[3] - x[4]))
}

new_row_business <- data.frame(t(data.frame(c("Business travel", "Sat-neutr/dissat",apply(Diff_means[3:16], 2, calc_diff_business)))))
print(new_row_business)

new_row_personal <- data.frame(t(data.frame(c("Personal Travel", "Sat-neutr/dissat",apply(Diff_means[3:16], 2, calc_diff_personal)))))
print(new_row_personal)

names(new_row_business) <- names(Diff_means)
names(new_row_personal) <- names(Diff_means)

new_row_business <- new_row_business %>% mutate_at(vars(3:16), as.numeric)
new_row_personal <- new_row_personal %>% mutate_at(vars(3:16), as.numeric)


Diff_means <- rbind(Diff_means, new_row_business, new_row_personal)

##t.test and anova.test on the most significant items that drive satisfaction among the two classes

Business <- train_cleaned %>% filter(Type.of.Travel == "Business travel")
Personal <- train_cleaned %>% filter(Type.of.Travel == "Personal Travel")

#Business

aov1 <- aov(Online.boarding ~ satisfaction, data = Business)
eta_squared(aov1)
anova(aov1)
TukeyHSD(aov1)

aov2 <- aov(Seat.comfort ~ satisfaction, data = Business)
eta_squared(aov2)
anova(aov2)


aov3 <- aov(Inflight.entertainment ~ satisfaction, data = Business)
eta_squared(aov3)
anova(aov3)

aov4 <- aov(On.board.service ~ satisfaction, data = Business)
eta_squared(aov4)
anova(aov4)

aov5 <- aov(Food.and.drink ~ satisfaction, data = Business)
eta_squared(aov5)
anova(aov5)

aov6 <- aov(Baggage.handling ~ satisfaction, data = Business)
eta_squared(aov6)
anova(aov6)

aov7 <- aov(Ease.of.Online.booking ~ satisfaction, data = Business)
eta_squared(aov7)
anova(aov7)


#Personal                                       

aov8 <- aov(Ease.of.Online.booking ~ satisfaction, data = Personal)
eta_squared(aov8)
anova(aov8)

aov9 <- aov(Online.boarding ~ satisfaction, data = Personal)
eta_squared(aov9)
anova(aov9)

aov10 <- aov(Inflight.wifi.service ~ satisfaction, data = Personal)
eta_squared(aov10)
anova(aov10)
                                                                    
lm(Personal$Arrival.Delay.in.Minute ~ satisfaction, data = Personal) %>% summary()
lm(Personal$Departure.Delay.in.Minute ~ satisfaction, data = Personal) %>% summary()

##EFA

nScree(train_cleaned[, 9:22])   ##3 out of 5 test suggest to use 4 factors for EFA
eigen(cor(train_cleaned[, 9:22]))$values  ##the fifth factors is slighlty under 1, which suggest that nFactors = 4 could  be the solution

factanal(train_cleaned[, 9:22], factors = 5)##the fifth factor has a low proportion var values and it only capture 1 item (Checkin.service), also the eingenvalue is under 1
## EFA with Factors = 4, finding the best rotation

factanal(train_cleaned[, 9:22], factors = 4)
factanal(train_cleaned[, 9:22], factors = 3)

factanal(train_cleaned[, 9:22], factors = 3, rotation = 'promax', param = 1, scores = "Bartlett")
factanal(train_cleaned[, 9:22], factors = 4, rotation = 'promax', param = 1, scores = "Bartlett") ##equamax rotation explain a little bit less variance, but factor 4 has more sense

fa <- factanal(train_cleaned[, 9:22], factors = 4, rotation = 'promax', param = 1, scores = "Bartlett")

factor_scores <- data.frame(fa$scores)

factor_scores <- factor_scores %>% rename("Enjoyment" = Factor1, "Service" = Factor2, "Ease_of_the_flight" = Factor3, "Online_boarding" = Factor4) 

final_df <- cbind(train_cleaned, factor_scores)


##K-means clustering 


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}  ## to obtain the ideal number of cluster for the k-mean clustering

wssplot(factor_scores)

kmeans(factor_scores, centers = 3)
kmeans(factor_scores, centers = 4)
kmeans(factor_scores, centers = 5)

clus <- as.data.frame(kmeans(factor_scores, centers = 4)$cluster) %>% 
  rename("clusters" = 'kmeans(factor_scores, centers = 4)$cluster')


k1 <- cbind(factor_scores, clus)


##test the significance of the clusters

anova(lm(Service ~ clusters, data = k1))  ##p.value = 0.3334
anova(lm(Enjoyment ~ clusters, data = k1))
anova(lm(Ease_of_the_flight ~ clusters, data = k1))


##EFA AND CLUSTERING FOR PERSONAL

##EFA

nScree(Personal[, 9:22])   ##2 out of 5 test suggest to use 5 factors for EFA
eigen(cor(Personal[, 9:22]))$values  ##the fifth factors is slighlty over 1, which suggest that nFactors = 4/5 could  be the solution

factanal(Personal[, 9:22], factors = 5)

## EFA with Factors = 4, finding the best rotation

factanal(Personal[, 9:22], factors = 3, rotation = 'promax', param = 1, scores = "Bartlett")  ##easy to read, but not good uniqueness values
factanal(Personal[, 9:22], factors = 4, rotation = 'promax', param = 1, scores = "Bartlett")
fa_p <- factanal(Personal[, 9:22], factors = 4, rotation = 'promax', param = 1, scores = "Bartlett") ##equamax rotation explain a little bit less variance, but factor 4 has more sense
factor_scores_p <- data.frame(fa_p$scores)

factor_scores_p <- factor_scores_p %>% rename("Enjoyment" = Factor1, "Digital_Services" = Factor2, "Service" = Factor3, "Ease_of_flight" = Factor4) 

final_df_p <- cbind(Personal, factor_scores_p)


##K-means clustering Personal

wssplot(factor_scores_p)

kmeans(factor_scores_p, centers = 4)  ##bad cluster distribution, clusters has no sense, half variance is not explained
kmeans(factor_scores_p, centers = 6)  ##good cluster distribution, clusters has no sense
kmeans(factor_scores_p, centers = 5)  ##cluster 2 has no sense

##It seems that no relevant clusters are obtained from clustering the Personal dataset

##EXPLORATION OF CLUSTERS OBTAINED

final_Df <- cbind(train_cleaned, factor_scores, k1[5])


clus_info <- final_Df %>% group_by(clusters, Type.of.Travel) %>% 
  summarize(Loyal = sum(Customer.Type == "Loyal Customer"),
            Disloyal = sum(Customer.Type == "disloyal Customer"),
            Loyal_prc = percent(Loyal/(Loyal + Disloyal)),
            Age = round(mean(Age), digits = 0),
            Eco = sum(Class == "Eco"),
            Eco.Plus = sum(Class == "Eco Plus"),
            Business = sum(Class == "Business"),
            Eco_prc = percent(Eco / (Eco + Eco.Plus+Business)),
            Eco.Plus_prc = percent(Eco.Plus / (Eco + Eco.Plus+Business)),
            Business_prc = percent(Business / (Eco + Eco.Plus+Business)),
            Flight.Distance = round(mean(Flight.Distance), digits = 0),
            Avrg_Delay = round(mean(Arrival.Delay.in.Minutes), digits = 0),
            Satisfied = sum(satisfaction == "satisfied"),
            Neutral.Dissatisfied = sum(satisfaction == "neutral or dissatisfied"),
            Satisfied_prc = percent(Satisfied / (Satisfied + Neutral.Dissatisfied)))



#Satisfaction drivers for each cluster

Diff_means_clusters <- final_Df %>% group_by(clusters, satisfaction) %>%
  summarize(Inflight.wifi.service = mean(Inflight.wifi.service),
            Gate.location = mean(Gate.location),
            Departure.Arrival.time.convenient = mean(Departure.Arrival.time.convenient),
            Ease.of.Online.booking = mean(Ease.of.Online.booking),
            Food.and.drink = mean(Food.and.drink),
            Online.boarding = mean(Online.boarding),
            Seat.comfort = mean(Seat.comfort),
            Inflight.entertainment = mean(Inflight.entertainment),
            On.board.service = mean(On.board.service),
            Leg.room.service = mean(Leg.room.service),
            Baggage.handling = mean(Baggage.handling),
            Checkin.service = mean(Checkin.service),
            Inflight.service = mean(Inflight.service), 
            Cleanliness = mean(Cleanliness))

write_xlsx(Diff_means_clusters, path = "Diff_means_clusters.xlsx")




