#Country of Origin

PD12dataframe<- as.data.frame(OSQ_daten$PD12_01)

PD12<- OSQ_daten$PD12_01
##################
Germany <- as.data.frame(str_count(PD12, "many"))

PD12dataframe$Germany<- Germany

sum(PD12dataframe$Germany == 1, na.rm = TRUE)#-->40

###############

Netherlands <- as.data.frame(str_count(PD12, "lands"))

PD12dataframe$Netherlands<- Netherlands

sum(PD12dataframe$Netherlands == 1, na.rm = TRUE)#-->6

###############

US <- as.data.frame(str_count(PD12, "US"))

PD12dataframe$US<- US

sum(PD12dataframe$US == 1, na.rm = TRUE) #-->29
#
UnitedStates <- as.data.frame(str_count(PD12, "tates"))

PD12dataframe$UnitedStates<- UnitedStates

sum(PD12dataframe$UnitedStates == 1, na.rm = TRUE)#-->9

##!!+2 Leute us und usa (kann man nicht eingeben weil Australia und so dann auch gezählt werden)


############

Australia <- as.data.frame(str_count(PD12, "tralia"))

PD12dataframe$Australia<- Australia

sum(PD12dataframe$Australia == 1, na.rm = TRUE)#-->3

###########

Switzerland <- as.data.frame(str_count(PD12, "Switzerland"))

PD12dataframe$Switzerland<- Switzerland

sum(PD12dataframe$Switzerland == 1, na.rm = TRUE)#-->7

###

Canada <- as.data.frame(str_count(PD12, "Canada"))

PD12dataframe$Canada<- Canada

sum(PD12dataframe$Canada == 1, na.rm = TRUE)#-->5

#############

UK <- as.data.frame(str_count(PD12, "UK"))

PD12dataframe$UK<- UK

sum(PD12dataframe$UK == 1, na.rm = TRUE)#-->10

#

UnitedKingdom <- as.data.frame(str_count(PD12, "ingdom"))

PD12dataframe$UnitedKingdom<- UnitedKingdom

sum(PD12dataframe$UnitedKingdom == 1, na.rm = TRUE)#-->3


#+ 2 Currently in UK Britain..../ uk

############

France <- as.data.frame(str_count(PD12, "rance"))

PD12dataframe$France<- France

sum(PD12dataframe$France == 1, na.rm = TRUE)#-->5
########

Italy <- as.data.frame(str_count(PD12, "taly"))

PD12dataframe$Italy<- Italy

sum(PD12dataframe$Italy == 1, na.rm = TRUE)#-->8

#################

Spain <- as.data.frame(str_count(PD12, "Spain"))

PD12dataframe$Spain<- Spain

sum(PD12dataframe$Spain == 1, na.rm = TRUE)#-->5

################

Portugal <- as.data.frame(str_count(PD12, "Portugal"))

PD12dataframe$Portugal<- Portugal
 
sum(PD12dataframe$Portugal == 1, na.rm = TRUE)# --> 2

###############

Belgium <- as.data.frame(str_count(PD12, "elgium"))

PD12dataframe$Denmark<- Belgium

sum(PD12dataframe$Belgium == 1, na.rm = TRUE)# --> 5

#+ 1 BELGIUM
##############

Norway <- as.data.frame(str_count(PD12, "Norway"))

PD12dataframe$Norway<- Norway

sum(PD12dataframe$Norway == 1, na.rm = TRUE)#--> 2

##############

Sweden <- as.data.frame(str_count(PD12, "Sweden"))

PD12dataframe$Sweden<- Sweden

sum(PD12dataframe$Sweden == 1, na.rm = TRUE) #--> 3

###########

Israel <- as.data.frame(str_count(PD12, "Israel"))

PD12dataframe$Israel<- Israel

sum(PD12dataframe$Israel == 1, na.rm = TRUE) #-->2

##########

Mexico <- as.data.frame(str_count(PD12, "Mexico"))

PD12dataframe$Mexico<- Mexico

sum(PD12dataframe$Mexico == 1, na.rm = TRUE) #--> 1


###########

Cyprus <- as.data.frame(str_count(PD12, "Cyprus"))

PD12dataframe$Cyprus<- Cyprus

sum(PD12dataframe$Cyprus == 1, na.rm = TRUE) #-->1

############

Portugal <- as.data.frame(str_count(PD12, "Portugal"))

PD12dataframe$Portugal<- Portugal

sum(PD12dataframe$Portugal == 1, na.rm = TRUE)-->2

############

India <- as.data.frame(str_count(PD12, "ndia"))

PD12dataframe$India<- India

sum(PD12dataframe$India == 1, na.rm = TRUE) #-->3


############

Japan <- as.data.frame(str_count(PD12, "Japan"))

PD12dataframe$Japan<- Japan

sum(PD12dataframe$Japan == 1, na.rm = TRUE)# --> 3

#############

Brazil <- as.data.frame(str_count(PD12, "Brazil"))

PD12dataframe$Brazil<- Brazil

sum(PD12dataframe$Brazil == 1, na.rm = TRUE)#-->3

##############


Poland <- as.data.frame(str_count(PD12, "Poland"))

PD12dataframe$Poland<- Poland

sum(PD12dataframe$Poland == 1, na.rm = TRUE)#-->1

###########

Morocco <- as.data.frame(str_count(PD12, "Morocco"))

PD12dataframe$Morocco<- Morocco

sum(PD12dataframe$Morocco == 1, na.rm = TRUE) #-->1


##########


Colombia <- as.data.frame(str_count(PD12, "Colombia"))

PD12dataframe$Colombia<- Colombia

sum(PD12dataframe$Colombia == 1, na.rm = TRUE) #-->1

##########

Wales <- as.data.frame(str_count(PD12, "Wales"))

PD12dataframe$Wales<- Wales

sum(PD12dataframe$Wales == 1, na.rm = TRUE)#-->1

#########

Czeck <- as.data.frame(str_count(PD12, "Czechia"))

PD12dataframe$Czeck<- Czeck

sum(PD12dataframe$Czeck == 1, na.rm = TRUE)#-->1

#########

Singapore <- as.data.frame(str_count(PD12, "Singapore"))

PD12dataframe$Singapore<- Singapore

sum(PD12dataframe$Singapore == 1, na.rm = TRUE)#-->1

##########

Greece <- as.data.frame(str_count(PD12, "Greece"))

PD12dataframe$Greece<- Greece

sum(PD12dataframe$Greece == 1, na.rm = TRUE)#-->1

#######

China <- as.data.frame(str_count(PD12, "China"))

PD12dataframe$China<- China

sum(PD12dataframe$China == 1, na.rm = TRUE)#-->1

########

Taiwan <- as.data.frame(str_count(PD12, "Taiwan"))

PD12dataframe$Taiwan<- Taiwan

sum(PD12dataframe$Taiwan == 1, na.rm = TRUE)#-->1

######

Ireland <- as.data.frame(str_count(PD12, "Ireland"))

PD12dataframe$Ireland<- Ireland

sum(PD12dataframe$Ireland == 1, na.rm = TRUE)#-->1

#####

Turkey <- as.data.frame(str_count(PD12, "Turkey"))

PD12dataframe$Turkey<- Turkey

sum(PD12dataframe$Turkey == 1, na.rm = TRUE)#-->1

######

Denmark <- as.data.frame(str_count(PD12, "Denmark"))

PD12dataframe$Denmark<- Denmark

sum(PD12dataframe$Denmark == 1, na.rm = TRUE)#-->1

######

SouthKorea <- as.data.frame(str_count(PD12, "South Korea"))

PD12dataframe$SouthKorea<- SouthKorea

sum(PD12dataframe$SouthKorea == 1, na.rm = TRUE)#-->1

######

NewZealand <- as.data.frame(str_count(PD12, "New Zealand"))

PD12dataframe$NewZealand<- NewZealand

sum(PD12dataframe$NewZealand == 1, na.rm = TRUE)#-->1











sum(PD12dataframe$Germany == 1, na.rm = TRUE)#-->40
sum(PD12dataframe$Netherlands == 1, na.rm = TRUE)#-->6
sum(PD12dataframe$US == 1, na.rm = TRUE) #-->29
sum(PD12dataframe$UnitedStates == 1, na.rm = TRUE)#-->9 + 2
sum(PD12dataframe$Australia == 1, na.rm = TRUE)#-->3
sum(PD12dataframe$Switzerland == 1, na.rm = TRUE)#-->7
sum(PD12dataframe$Canada == 1, na.rm = TRUE)#-->5
sum(PD12dataframe$UK == 1, na.rm = TRUE)#-->10 +2
sum(PD12dataframe$UnitedKingdom == 1, na.rm = TRUE)#-->3
sum(PD12dataframe$France == 1, na.rm = TRUE)#-->5
sum(PD12dataframe$Italy == 1, na.rm = TRUE)#-->8
sum(PD12dataframe$Spain == 1, na.rm = TRUE)#-->5
sum(PD12dataframe$Portugal == 1, na.rm = TRUE)# --> 2
sum(PD12dataframe$Belgium == 1, na.rm = TRUE)# --> 5 +1
sum(PD12dataframe$Norway == 1, na.rm = TRUE)#--> 2
sum(PD12dataframe$Sweden == 1, na.rm = TRUE) #--> 3
sum(PD12dataframe$Israel == 1, na.rm = TRUE) #-->2
sum(PD12dataframe$Mexico == 1, na.rm = TRUE) #--> 1
sum(PD12dataframe$Cyprus == 1, na.rm = TRUE) #-->1
sum(PD12dataframe$India == 1, na.rm = TRUE) #-->3
sum(PD12dataframe$Japan == 1, na.rm = TRUE)# --> 3
sum(PD12dataframe$Brazil == 1, na.rm = TRUE)#-->3
sum(PD12dataframe$Poland == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Morocco == 1, na.rm = TRUE) #-->1
sum(PD12dataframe$Colombia == 1, na.rm = TRUE) #-->1
sum(PD12dataframe$Wales == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Czeck == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Singapore == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Greece == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$China == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Taiwan == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Ireland == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Turkey == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$Denmark == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$SouthKorea == 1, na.rm = TRUE)#-->1
sum(PD12dataframe$NewZealand == 1, na.rm = TRUE)#-->1
# -->174

sum(is.na(OSQ_daten$PD12_01)) # -->109 not answered
