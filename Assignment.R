###Reading CSV file
Census <- read.csv(file.choose())



###Creating subsets for CD Blocks, Towns and Villages
attach(Census)
CDBlocks <- subset(Census, Level=="CD BLOCK" & Total.Rural.Urban=="Total")
CDBlocksN <- subset(Census, Level=="CD BLOCK" & Total.Rural.Urban!="Total")
Town <- subset(Census, Level=="TOWN")
Village <- subset(Census, Level=="VILLAGE")
detach(Census)


###Creating Categories in those subsets
attach(CDBlocks)
CDCat <- ifelse(Total.Population.Person <= 200000, 'A',
                ifelse(Total.Population.Person > 200000 & Total.Population.Person <= 500000, 'B',
                       ifelse(Total.Population.Person > 500000, 'C','F')))

CDBlockCat <- data.frame(CDBlocks,CDCat)
detach(CDBlocks)

attach(Town)
TCat <- ifelse(Total.Population.Person <= 15000, "T1",
               ifelse(Total.Population.Person > 15000 & Total.Population.Person <= 25000, "T2",
                      ifelse(Total.Population.Person > 25000, "T3", "T9")))

TownCat <- data.frame(Town,TCat)
detach(Town)


attach(Village)
VCat <- ifelse(Total.Population.Person <= 2500, "V1",
               ifelse(Total.Population.Person > 2500 & Total.Population.Person <= 10000, "V2", 
                      ifelse(Total.Population.Person > 10000 & Total.Population.Person <= 40000, "V3",
                             ifelse(Total.Population.Person > 40000, "V4", "V9"))))

VillageCat <- data.frame(Village,VCat)
detach(Village)

myvars <- c("CD.Block_Code", "CDCat")
CatCD <- CDBlockCat[myvars]
CDBlockCat2 <- merge(CDBlocksN, CatCD, by="CD.Block_Code")



###Calculating Ratios  

####CD Block Ratios


library("plyr", lib.loc="~/R/win-library/3.3")

CDBlockRatios <- ddply(CDBlockCat2,"CDCat",summarize,SexRatio=((sum(Total.Population.Female)/sum(Total.Population.Male))*1000),
                       ChildSexRatio=((sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Male))*1000),
                       AvgLiteracyRate=((mean(Literates.Population.Person)/mean(Total.Population.Person - Population.in.the.age.group.0.6.Person))*100),
                       FemaleLiteracyRate=((sum(Literates.Population.Female)/sum(Total.Population.Female - Population.in.the.age.group.0.6.Female))*100),
                       ChildProportion=(sum(Population.in.the.age.group.0.6.Person)/sum(Total.Population.Person)),
                       GirlsProportion=(sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Person)),
                       AvgPercentFemaleWorker=((mean(Total.Worker.Population.Female)/mean(Total.Worker.Population.Person))*100),
                       MainMarginalWorkerFemale=sum(Main.Working.Population.Female)/sum(Marginal.Worker.Population.Female),
                       CultivatorToWorker=(sum(Main.Cultivator.Population.Female)/sum(Main.Working.Population.Female)),
                       AgriLabourToWorker=(sum(Main.Agricultural.Labourers.Population.Female)/sum(Main.Working.Population.Female)),
                       HouseholdToWorker=(sum(Main.Household.Industries.Population.Female)/sum(Main.Working.Population.Female)),
                       OtherToWorker=(sum(Main.Other.Workers.Population.Female)/sum(Main.Working.Population.Female)),
                       AvgChildPerHousehold=(mean(Population.in.the.age.group.0.6.Person)/mean(No.of.Households)),
                       AvgGirlChildPerHousehold=(mean(Population.in.the.age.group.0.6.Female)/mean(No.of.Households)),
                       AvgNonWorkingPopFemale=((mean(Non.Working.Population.Female)/mean(Non.Working.Population.Person))*100))

#CD Block Ratios
CDBlockRatios



####Town Ratios
TownRatios <- ddply(TownCat,"TCat",summarize,SexRatio=((sum(Total.Population.Female)/sum(Total.Population.Male))*1000),
                    ChildSexRatio=((sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Male))*1000),
                    AvgLiteracyRate=((mean(Literates.Population.Person)/mean(Total.Population.Person - Population.in.the.age.group.0.6.Person))*100),
                    FemaleLiteracyRate=((sum(Literates.Population.Female)/sum(Total.Population.Female - Population.in.the.age.group.0.6.Female))*100),
                    ChildProportion=(sum(Population.in.the.age.group.0.6.Person)/sum(Total.Population.Person)),
                    GirlsProportion=(sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Person)),
                    AvgPercentFemaleWorker=((mean(Total.Worker.Population.Female)/mean(Total.Worker.Population.Person))*100),
                    MainMarginalWorkerFemale=sum(Main.Working.Population.Female)/sum(Marginal.Worker.Population.Female),
                    CultivatorToWorker=(sum(Main.Cultivator.Population.Female)/sum(Main.Working.Population.Female)),
                    AgriLabourToWorker=(sum(Main.Agricultural.Labourers.Population.Female)/sum(Main.Working.Population.Female)),
                    HouseholdToWorker=(sum(Main.Household.Industries.Population.Female)/sum(Main.Working.Population.Female)),
                    OtherToWorker=(sum(Main.Other.Workers.Population.Female)/sum(Main.Working.Population.Female)),
                    AvgChildPerHousehold=(mean(Population.in.the.age.group.0.6.Person)/mean(No.of.Households)),
                    AvgGirlChildPerHousehold=(mean(Population.in.the.age.group.0.6.Female)/mean(No.of.Households)),
                    AvgNonWorkingPopFemale=((mean(Non.Working.Population.Female)/mean(Non.Working.Population.Person))*100))

#Town Ratios
TownRatios


####Village Ratios

VillageRatios <- ddply(VillageCat,"VCat",summarize,SexRatio=((sum(Total.Population.Female)/sum(Total.Population.Male))*1000),
                       ChildSexRatio=((sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Male))*1000),
                       AvgLiteracyRate=((mean(Literates.Population.Person)/mean(Total.Population.Person - Population.in.the.age.group.0.6.Person))*100),
                       FemaleLiteracyRate=((sum(Literates.Population.Female)/sum(Total.Population.Female - Population.in.the.age.group.0.6.Female))*100),
                       ChildProportion=(sum(Population.in.the.age.group.0.6.Person)/sum(Total.Population.Person)),
                       GirlsProportion=(sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Person)),
                       AvgPercentFemaleWorker=((mean(Total.Worker.Population.Female)/mean(Total.Worker.Population.Person))*100),
                       MainMarginalWorkerFemale=sum(Main.Working.Population.Female)/sum(Marginal.Worker.Population.Female),
                       CultivatorToWorker=(sum(Main.Cultivator.Population.Female)/sum(Main.Working.Population.Female)),
                       AgriLabourToWorker=(sum(Main.Agricultural.Labourers.Population.Female)/sum(Main.Working.Population.Female)),
                       HouseholdToWorker=(sum(Main.Household.Industries.Population.Female)/sum(Main.Working.Population.Female)),
                       OtherToWorker=(sum(Main.Other.Workers.Population.Female)/sum(Main.Working.Population.Female)),
                       AvgChildPerHousehold=(mean(Population.in.the.age.group.0.6.Person)/mean(No.of.Households)),
                       AvgGirlChildPerHousehold=(mean(Population.in.the.age.group.0.6.Female)/mean(No.of.Households)),
                       AvgNonWorkingPopFemale=((mean(Non.Working.Population.Female)/mean(Non.Working.Population.Person))*100))

#Village Ratios
VillageRatios



####Rural vs Urban Ratios

RuralUrbanRatios <- ddply(CDBlockCat2,"Total.Rural.Urban",summarize,SexRatio=((sum(Total.Population.Female)/sum(Total.Population.Male))*1000),
                          ChildSexRatio=((sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Male))*1000),
                          AvgLiteracyRate=((mean(Literates.Population.Person)/mean(Total.Population.Person))*100),
                          FemaleLiteracyRate=((sum(Literates.Population.Female)/sum(Total.Population.Female))*100),
                          ChildProportion=(sum(Population.in.the.age.group.0.6.Person)/sum(Total.Population.Person)),
                          GirlsProportion=(sum(Population.in.the.age.group.0.6.Female)/sum(Population.in.the.age.group.0.6.Person)),
                          AvgPercentFemaleWorker=((mean(Total.Worker.Population.Female)/mean(Total.Worker.Population.Person))*100),
                          MainMarginalWorkerFemale=sum(Main.Working.Population.Female)/sum(Marginal.Worker.Population.Female),
                          CultivatorToWorker=(sum(Main.Cultivator.Population.Female)/sum(Main.Working.Population.Female)),
                          AgriLabourToWorker=(sum(Main.Agricultural.Labourers.Population.Female)/sum(Main.Working.Population.Female)),
                          HouseholdToWorker=(sum(Main.Household.Industries.Population.Female)/sum(Main.Working.Population.Female)),
                          OtherToWorker=(sum(Main.Other.Workers.Population.Female)/sum(Main.Working.Population.Female)),
                          AvgChildPerHousehold=(mean(Population.in.the.age.group.0.6.Person)/mean(No.of.Households)),
                          AvgGirlChildPerHousehold=(mean(Population.in.the.age.group.0.6.Female)/mean(No.of.Households)),
                          AvgNonWorkingPopFemale=((mean(Non.Working.Population.Female)/mean(Non.Working.Population.Person))*100))

#Rural vs Urban Ratios
RuralUrbanRatios




library("ggplot2", lib.loc="~/R/win-library/3.3")
attach(CDBlockRatios)

qplot(CDCat, SexRatio, data=CDBlockRatios, xlab="CD Block Category", ylab="Sex Ratio", main="CD Block Category vs Sex Ratio", ylim=c(800,1000))
qplot(CDCat, ChildSexRatio, data=CDBlockRatios, xlab="CD Block Category", ylab="Child Sex Ratio", main="CD Block Category vs Child Sex Ratio", ylim=c(800,1000))
qplot(CDCat, FemaleLiteracyRate, data=CDBlockRatios, xlab="CD Block Category", ylab="Female Literacy Rate", main="CD Block Category vs Female Literacy Rate", ylim=c(0,100))
qplot(CDCat, GirlsProportion, data=CDBlockRatios, xlab="CD Block Category", ylab="Proportion of Girls", main="CD Block Category vs Proportion of Girls(0-6 years)", ylim=c(0,1))
qplot(CDCat, AvgPercentFemaleWorker, data=CDBlockRatios, xlab="CD Block Category", ylab="Average Percentage of Female Workers", main="CD Block Category vs Average Percentage of Female Workers", ylim=c(0,100))
qplot(CDCat, AvgGirlChildPerHousehold, data=CDBlockRatios, xlab="CD Block Category", ylab="Average Number of Girl Child per Household", main="CD Block Category vs Average Number of Girl Child per Household", ylim=c(0,3))
qplot(CDCat, AvgNonWorkingPopFemale, data=CDBlockRatios, xlab="CD Block Category", ylab="Average Percentage of Non-Working Population of Females", main="CD Block Category vs Average Percentage of Non-Working Population of Females", ylim=c(0,100))

detach(CDBlockRatios)


attach(TownRatios)

qplot(TCat, SexRatio, data=TownRatios, xlab="Town Category", ylab="Sex Ratio", main="Town Category vs Sex Ratio", ylim=c(800,1000))
qplot(TCat, ChildSexRatio, data=TownRatios, xlab="Town Category", ylab="Child Sex Ratio", main="Town Category vs Child Sex Ratio", ylim=c(800,1000))
qplot(TCat, FemaleLiteracyRate, data=TownRatios, xlab="Town Category", ylab="Female Literacy Rate", main="Town Category vs Female Literacy Rate", ylim=c(0,100))
qplot(TCat, GirlsProportion, data=TownRatios, xlab="Town Category", ylab="Proportion of Girls", main="Town Category vs Proportion of Girls(0-6 years)", ylim=c(0,1))
qplot(TCat, AvgPercentFemaleWorker, data=TownRatios, xlab="Town Category", ylab="Average Percentage of Female Workers", main="Town Category vs Average Percentage of Female Workers", ylim=c(0,100))
qplot(TCat, AvgGirlChildPerHousehold, data=TownRatios, xlab="Town Category", ylab="Average Number of Girl Child per Household", main="Town Category vs Average Number of Girl Child per Household", ylim=c(0,3))
qplot(TCat, AvgNonWorkingPopFemale, data=TownRatios, xlab="Town Category", ylab="Average Percentage of Non-Working Population of Females", main="Town Category vs Average Percentage of Non-Working Population of Females", ylim=c(0,100))

detach(TownRatios)


attach(VillageRatios)

qplot(VCat, SexRatio, data=VillageRatios, xlab="Village Category", ylab="Sex Ratio", main="Village Category vs Sex Ratio", ylim=c(800,1000))
qplot(VCat, ChildSexRatio, data=VillageRatios, xlab="Village Category", ylab="Child Sex Ratio", main="Village Category vs Child Sex Ratio", ylim=c(800,1000))
qplot(VCat, FemaleLiteracyRate, data=VillageRatios, xlab="Village Category", ylab="Female Literacy Rate", main="Village Category vs Female Literacy Rate", ylim=c(0,100))
qplot(VCat, GirlsProportion, data=VillageRatios, xlab="Village Category", ylab="Proportion of Girls", main="Village Category vs Proportion of Girls(0-6 years)", ylim=c(0,1))
qplot(VCat, AvgPercentFemaleWorker, data=VillageRatios, xlab="Village Category", ylab="Average Percentage of Female Workers", main="Village Category vs Average Percentage of Female Workers", ylim=c(0,100))
qplot(VCat, AvgGirlChildPerHousehold, data=VillageRatios, xlab="Village Category", ylab="Average Number of Girl Child per Household", main="Village Category vs Average Number of Girl Child per Household", ylim=c(0,3))
qplot(VCat, AvgNonWorkingPopFemale, data=VillageRatios, xlab="Village Category", ylab="Average Percentage of Non-Working Population of Females", main="Village Category vs Average Percentage of Non-Working Population of Females", ylim=c(0,100))

detach(VillageRatios)


attach(RuralUrbanRatios)

qplot(Total.Rural.Urban, AvgLiteracyRate, data=RuralUrbanRatios, xlab="Rural/Urban", ylab="Average Literacy Rate", main="Rural/Urban vs Average Literacy Rate")
qplot(Total.Rural.Urban, ChildSexRatio, data=RuralUrbanRatios, xlab="Rural/Urban", ylab="Child Sex Ratio (Females per 1000 Males)", main="Rural/Urban vs Child Sex Ratio")
qplot(Total.Rural.Urban, FemaleLiteracyRate, data=RuralUrbanRatios, xlab="Rural/Urban", ylab="Female Literacy Rate", main="Rural/Urban vs Female Literacy Rate")
qplot(Total.Rural.Urban, AvgPercentFemaleWorker, data=RuralUrbanRatios, xlab="Rural/Urban", ylab="Average Percentage of Female Worker Population", main="Rural/Urban vs Average Percentage of Female Worker Population")

detach(RuralUrbanRatios)


VFemaleLiteracyRate <- VillageRatios$FemaleLiteracyRate
VAvgPercentWorkingPopFemale <- VillageRatios$AvgPercentFemaleWorker
cor(VFemaleLiteracyRate, VAvgPercentWorkingPopFemale)


####Rural vs Urban Sex Ratio  
RuralUrbanRatios[1:2]


####Pune Population Density
PunePopulationDensity <- sum(CDBlockCat2$Total.Population.Person)/15643
PunePopulationDensity


####Top 5 Literacy Rates  

##**Top 5 CD Blocks based on Literacy Rates**
  
#Top 5 CD Block Literacy Rates
LiteracyRate <- (CDBlockCat$Literates.Population.Person/CDBlockCat$Total.Population.Person)*100
CDBlockCat <- data.frame(CDBlockCat, LiteracyRate)
Top5LiteracyRateCDBlock <- head(arrange(CDBlockCat,desc(LiteracyRate)), n = 5)
Top5LiteracyRateCDBlock[c(3,10,97,98)]


##**Top 5 Towns based on Literacy Rates**

#Top 5 Town Literacy Rates
LiteracyRate <- (TownCat$Literates.Population.Person/TownCat$Total.Population.Person)*100
TownCat <- data.frame(TownCat, LiteracyRate)
Top5LiteracyRateTown <- head(arrange(TownCat,desc(LiteracyRate)), n = 5)
Top5LiteracyRateTown[c(3,10,97,98)]


##**Top 5 Villages based on Literacy Rates**

#Top 5 Village Literacy Rates
LiteracyRate <- (VillageCat$Literates.Population.Person/VillageCat$Total.Population.Person)*100
VillageCat <- data.frame(VillageCat, LiteracyRate)
Top5LiteracyRateVillage <- head(arrange(VillageCat,desc(LiteracyRate)), n = 5)
Top5LiteracyRateVillage[c(3,10,97,98)]


####Top 5 Female Literacy Rates

##**Top 5 CD Blocks based on Female Literacy Rates**

#Top 5 CD Block Female Literacy Rates
LiteracyRateFemale <- (CDBlockCat$Literates.Population.Female/CDBlockCat$Total.Population.Female)*100
CDBlockCat <- data.frame(CDBlockCat, LiteracyRateFemale)
Top5FemaleLiteracyRateCDBlock <- head(arrange(CDBlockCat,desc(LiteracyRateFemale)), n = 5)
Top5FemaleLiteracyRateCDBlock[c(3,10,97,99)]



#**Top 5 Towns based on Female Literacy Rates**

#Top 5 Town Female Literacy Rates
LiteracyRateFemale <- (TownCat$Literates.Population.Female/TownCat$Total.Population.Female)*100
TownCat <- data.frame(TownCat, LiteracyRateFemale)
Top5FemaleLiteracyRateTown <- head(arrange(TownCat,desc(LiteracyRateFemale)), n = 5)
Top5FemaleLiteracyRateTown[c(3,10,97,99)]



#**Top 5 Villages based on Female Literacy Rates**

#Top 5 Village Female Literacy Rates
LiteracyRateFemale <- (VillageCat$Literates.Population.Female/VillageCat$Total.Population.Female)*100
VillageCat <- data.frame(VillageCat, LiteracyRateFemale)
Top5FemaleLiteracyRateVillage <- head(arrange(VillageCat,desc(LiteracyRateFemale)), n = 5)
Top5FemaleLiteracyRateVillage[c(3,10,97,99)]


####Bottom 5 Literacy Rates

#**Bottom 5 CD Blocks based on Literacy Rates**

#Bottom 5 CD Block Literacy Rates
Bottom5LiteracyRateCDBlock <- head(arrange(CDBlockCat,LiteracyRate), n = 5)
Bottom5LiteracyRateCDBlock[c(3,10,97,98)]



#**Bottom Towns based on Literacy Rates**

#Bottom 5 Town Literacy Rates
Bottom5LiteracyRateTown <- head(arrange(TownCat,LiteracyRate), n = 5)
Bottom5LiteracyRateTown[c(3,10,97,98)]



#**Bottom 5 Villages based on Literacy Rates**

#Bottom 5 Village Literacy Rates
Bottom5LiteracyRateVillage <- head(arrange(VillageCat,LiteracyRate), n = 5)
Bottom5LiteracyRateVillage[c(3,10,97,98)]


####Bottom 5 Female Literacy Rates

#**Bottom 5 CD Blocks based on Female Literacy Rates**

#Bottom 5 CD Block Female Literacy Rates
Bottom5FemaleLiteracyRateCDBlock <- head(arrange(CDBlockCat,LiteracyRateFemale), n = 5)
Bottom5FemaleLiteracyRateCDBlock[c(3,10,97,99)]



#**Bottom 5 CD Blocks based on Female Literacy Rates**

#Bottom 5 Town Female Literacy Rates
Bottom5FemaleLiteracyRateTown <- head(arrange(TownCat,LiteracyRateFemale), n = 5)
Bottom5FemaleLiteracyRateTown[c(3,10,97,99)]



#**Bottom 5 CD Blocks based on Female Literacy Rates**

#Bottom 5 Village Female Literacy Rates
Bottom5FemaleLiteracyRateVillage <- head(arrange(VillageCat,LiteracyRateFemale), n = 5)
Bottom5FemaleLiteracyRateVillage[c(3,10,97,99)]


####Category Wise Lowest Sex Ratios

#**Category Wise Lowest Sex Ratios in CD Blocks**

#Lowest Sex Ratio CD Blocks
SexRatio <- (CDBlockCat$Total.Population.Female/CDBlockCat$Total.Population.Male)*1000
CDBlockCat <- data.frame(CDBlockCat, SexRatio)
CDBlockCat1 <- subset(CDBlockCat, CDCat=="A")
CDBlockCat2 <- subset(CDBlockCat, CDCat=="B")
CDBlockCat3 <- subset(CDBlockCat, CDCat=="C")
LowestSexRatioCDBlockA <- head(arrange(CDBlockCat1,SexRatio), n = 1)
LowestSexRatioCDBlockA[c(3,10,97,100)]
LowestSexRatioCDBlockB <- head(arrange(CDBlockCat2,SexRatio), n = 1)
LowestSexRatioCDBlockB[c(3,10,97,100)]
LowestSexRatioCDBlockC <- head(arrange(CDBlockCat3,SexRatio), n = 1)
LowestSexRatioCDBlockC[c(3,10,97,100)]


#**Category Wise Lowest Sex Ratios in Towns**

#Lowest Sex Ratio Towns
SexRatio <- (TownCat$Total.Population.Female/TownCat$Total.Population.Male)*1000
TownCat <- data.frame(TownCat, SexRatio)
TownCat1 <- subset(TownCat, TCat=="T1")
TownCat2 <- subset(TownCat, TCat=="T2")
TownCat3 <- subset(TownCat, TCat=="T3")
LowestSexRatioTownT1 <- head(arrange(TownCat1,SexRatio), n = 1)
LowestSexRatioTownT1[c(3,9,10,97,100)]
LowestSexRatioTownT2 <- head(arrange(TownCat2,SexRatio), n = 1)
LowestSexRatioTownT2[c(3,9,10,97,100)]
LowestSexRatioTownT3 <- head(arrange(TownCat3,SexRatio), n = 1)
LowestSexRatioTownT3[c(3,9,10,97,100)]


#**Category Wise Lowest Sex Ratios in Villages**

#Lowest Sex Ratio Villages
SexRatio <- (VillageCat$Total.Population.Female/VillageCat$Total.Population.Male)*1000
VillageCat <- data.frame(VillageCat, SexRatio)
VillageCat1 <- subset(VillageCat, VCat=="V1")
VillageCat2 <- subset(VillageCat, VCat=="V2")
VillageCat3 <- subset(VillageCat, VCat=="V3")
VillageCat4 <- subset(VillageCat, VCat=="V4")
LowestSexRatioVillageV1 <- head(arrange(VillageCat1,SexRatio), n = 1)
LowestSexRatioVillageV1[c(3,9,10,97,100)]
LowestSexRatioVillageV2 <- head(arrange(VillageCat2,SexRatio), n = 1)
LowestSexRatioVillageV2[c(3,9,10,97,100)]
LowestSexRatioVillageV3 <- head(arrange(VillageCat3,SexRatio), n = 1)
LowestSexRatioVillageV3[c(3,9,10,97,100)]
LowestSexRatioVillageV4 <- head(arrange(VillageCat4,SexRatio), n = 1)
LowestSexRatioVillageV4[c(3,9,10,97,100)]


#End of File