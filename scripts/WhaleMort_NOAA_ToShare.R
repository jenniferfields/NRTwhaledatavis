rm(list=ls()); w=3; h=3; w2=4.5; w3=7.5; h2=4.5

# defining working directories for data & graphs
wd <- "/Volumes/AThomsen Dr/WhaleMort/Technical/NOAA/Data/CSVs"
wd_graphs <- "/Volumes/AThomsen Dr/WhaleMort/Technical/NOAA/Graphs" 

setwd(wd)

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# function to help rename species in a consistent way across data frames
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

# making color scale for species (so each species always gets graphed using a consistent color)
mycolors = brewer.pal(9,"Spectral")
names(mycolors) = c("Blue","Fin","Gray","Humpback","Minke","Orca","Sei","Sperm","Unknown")
colScale <- scale_fill_manual(name = "Common.Name",values = mycolors)


###### ENTANGLEMENTS ANALYSIS ######

en = read.csv("Entangled_WA-OR-CA_1982-2019_corrected.csv",stringsAsFactors = F)
en$Response.Date = as.Date(en$Response.Date,format="%m/%d/%y") # format date

###### examining data
colnames(en)
unique(en$Species) # gray, humpback, unknown, minke, fin, orca, blue, sperm
range(en$Response.Date, na.rm=T) # 1982 - 2019
nrow(en[en$Confirmed.Entanglement=="Confirmed Entangled",]) # 501 (all) confirmed
unique(en$County) # 30 counties
unique(en$State) # California, Washington, Oregon, 12 blank (international)
unique(en$Vital.Status) # 9 not recorded as Alive or Dead
nrow(en[en$Vital.Status=="Alive",]) # 417 Alive
nrow(en[en$Vital.Status=="Dead",]) # 75 Dead
unique(en$Entanglement.Type) # Fishery (287), Other (2), Unknown (212)
unique(en$Entanglement.Fishery.Certainty) # Confirmed (274), Unconfirmed (27), blank
unique(en$Entanglement.Fishery.Type) # 10 fishery types plus Unknown, Other, blank

# adding new columns
en$Common.Name=gsub(" Whale","",en$Species) # making column of shorter names
en$Common.Name[en$Common.Name=="Killer"] <- "Orca"
en$Year=format(as.Date(en$Response.Date,format="%Y-%m-%d"),"%Y")
en$Month=format(as.Date(en$Response.Date,format="%Y-%m-%d"),"%m")

# filling in County for CAN/MEX cases with the country so it isn't blank
en$County[en$Country%in%c("CAN","MEX")] <- en$Country[en$Country%in%c("CAN","MEX")]

# replacing blanks with "Unknown"
en$Entanglement.Fishery.Type[en$Entanglement.Fishery.Type==""] <- "Unknown"

# making dataset of 2010-present only
en2010 = en[en$Year>=2010,]

# adding column to sort counties geographically
counties = data.frame(County = c("CAN","San Juan","Island","Clallam","Snohomish",
                                 "Jefferson","King","Grays Harbor","Pacific",
                                 "Clatsop","Tillamook","Lincoln","Douglas","Coos","Curry",
                                 "Del Norte","Humboldt","Mendocino","Sonoma","Marin",
                                 "San Francisco","San Mateo","Santa Cruz","Monterey",
                                 "San Luis Obispo","Santa Barbara","Ventura","Los Angeles",
                                 "Orange","San Diego","MEX"),
                      Sort = 1:31)
en = merge(en, counties)
counties2010 = data.frame(County = as.character(counties[counties$County%in%en2010$County,"County"]),
                          Sort = 1:length(unique(en2010$County)))
en2010 = merge(en2010, counties2010)

# stats to include in writing
nrow(en2010)/length(unique(en2010$Year)) # mean annual entanglements = 28.4
nrow(en2010[en2010$Vital.Status=="Alive",])/nrow(en2010)*100 # 89.4% of cases were "Alive"
nrow(en2010[en2010$Entanglement.Fishery.Type=="Unknown",])/nrow(en2010)*100 
  # 52.8% of cases had unknown gear
en2010_knownsp = nrow(en2010[en2010$Common.Name!="Unknown",]) # 274 cases were identified to species
nrow(en2010[en2010$Common.Name=="Gray",])/en2010_knownsp*100 # gray = 28.8% of identified cases
nrow(en2010[en2010$Common.Name=="Humpback",])/en2010_knownsp*100 # humpback = 64.6% of identified cases
en2010_knowngear = en2010[en2010$Entanglement.Fishery.Type!="Unknown",] # dataset where gear is known
nrow(en2010_knowngear[
  which(en2010_knowngear$Entanglement.Fishery.Type=="ComDungCrab"&
    en2010_knowngear$Common.Name=="Humpback"),])/
  nrow(en2010_knowngear[en2010_knowngear$Common.Name=="Humpback",])*100 
  # 62.9% of humpback cases w/ identifiable gear were ComDungCrab
nrow(en2010_knowngear[en2010_knowngear$Common.Name=="Humpback",]) # 89 humpbacks w/ identifiable gear
nrow(en2010_knowngear[
  which(en2010_knowngear$Entanglement.Fishery.Type=="Gillnet"&
          en2010_knowngear$Common.Name=="Gray"),])/
  nrow(en2010_knowngear[en2010_knowngear$Common.Name=="Gray",])*100 
  # 48.6% of gray cases w/ identifiable gear were Gillnet
nrow(en2010_knowngear[en2010_knowngear$Common.Name=="Gray",]) # 35 grays w/ identifiable gear
nrow(en2010[en2010$Common.Name=="Minke",]) # 3 minke in post-2010 dataset
nrow(en2010[en2010$Common.Name=="Orca",]) # 2 orcas in post-2010 dataset


###### graphs
setwd(wd_graphs)

# bar graph w/ entanglements by year (color by species)
en.sp.year =
  en %>%
  group_by(Common.Name,Year) %>%
  tally()

quartz(height=h,width=w3)
ggplot(en.sp.year, aes(x = Year, y = n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("Year") +
  ylab("Total entanglements") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))
dev.copy(dev=png,file="Entangl_year_species_allyrs.png",wid=w3,hei=h,
         uni="in",res=450)
dev.off()

# bar graph w/ entanglements by year (color by vital status)
en.vs.year =
  en %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w3)
ggplot(en.vs.year, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Set3") +
  xlab("Year") +
  ylab("Total entanglements") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))

# bar graph w/ entanglements by month (color by species)
en.sp.mo =
  en2010 %>%
  group_by(Common.Name,Month) %>%
  tally()

quartz(height=h,width=w2)
ggplot(en.sp.mo, aes(x = Month, y = n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("Month") +
  ylab("Entangl. 2010-present") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=45, vjust=0.5))
dev.copy(dev=png,file="Entangl_month_species_post2010.png",wid=w2,hei=h,
         uni="in",res=450)
dev.off()

# bar graph w/ entanglements by county (color by species)
en.sp.county =
  en2010 %>%
  group_by(Common.Name,Sort) %>%
  tally()

quartz(height=h,width=w3)
ggplot(en.sp.county, aes(x = Sort, y = n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("County/Country") +
  ylab("Entangl. 2010-present") +
  scale_x_discrete(limits=counties2010$County) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
dev.copy(dev=png,file="Entangl_county_species_post2010.png",wid=w3,hei=h,
         uni="in",res=450)
dev.off()

# bar graph w/ entanglements by gear type (color by species)
en.sp.gear = 
  en2010 %>%
  group_by(Common.Name,Entanglement.Fishery.Type) %>%
  tally()

quartz(height=h,width=w2)
ggplot(en.sp.gear, aes(x=Entanglement.Fishery.Type, y=n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("Gear type") +
  ylab("Entangl. 2010-present") +
  theme_classic() +
  theme(legend.title = element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
dev.copy(dev=png,file="Entangl_gear_species_post2010.png",wid=w2,hei=h,
         uni="in",res=450)
dev.off()


###### STRANDINGS ######
setwd(wd)

st_WAOR = read.csv("Strands_WA-OR_1974-2019.csv",stringsAsFactors = F)
st_CA_pre2006 = read.csv("Strands_CA_1982-2005.csv",stringsAsFactors = F)
st_CA_post2006 = read.csv("Strands_CA_2006-2019.csv",stringsAsFactors = F)

# checking colnames between WA-OR and CA post-2006 are the same
cols_all = colnames(st_WAOR)
colnames(st_CA_post2006)%in%cols_all # all "TRUE"

# columns of interest - to be checked
# National.Database.Number
colnames(st_WAOR)[1] <- "National.Database.Number" # fixing weird error
colnames(st_CA_post2006)[1] <- "National.Database.Number" # fixing weird error
# Common.name
# State
# County
# Findings.of.Human.Interaction
# Boat.Collision
# Shot
# Fishery.Interaction
# Fishery.Type
# Other.Human.Interaction
# Other.Human.Int.Description
# Other.Human.Int.Type
# possibly Other.Findings categories... illness, injury, etc (or Sick and Injured columns)
# Observation.Date (rather than Date.of.Examination or Date.of.Initial.Disposition)
# Condition.at.Examination (rather than Observation.Status)
# Necropsied
cols_ofinterest = c("National.Database.Number","Common.Name","State","County",
                    "Findings.of.Human.Interaction","Boat.Collision","Shot","Fishery.Interaction",
                    "Fishery.Type","Other.Human.Interaction","Other.Human.Int.Description",
                    "Other.Human.Int.Type","Condition.at.Examination","Necropsied","Observation.Date")

# checking WA-OR dataset
# notes: Fishery.Type & Other.Human.Int.Type not useful (all NA)
st_WAOR$Observation.Date = as.Date(st_WAOR$Observation.Date,format="%m/%d/%y") # format date
range(st_WAOR$Observation.Date) # 1974 - 2020
unique(st_WAOR$Common.Name) # 14 categories incl. multiple unknown/unidentified
unique(st_WAOR$State) # WA & OR
unique(st_WAOR$County) # 24 listed -- Jefferson & Jefferson County to be combined
unique(st_WAOR$Findings.of.Human.Interaction) # CBD, Y, N
unique(st_WAOR$Boat.Collision) # N, Y, C, ""
unique(st_WAOR$Shot) # N, Y, C, ""
unique(st_WAOR$Fishery.Interaction) # N, Y, C, ""
unique(st_WAOR$Fishery.Type) # NA
unique(st_WAOR$Other.Human.Interaction) # N, Y, ""
unique(st_WAOR$Other.Human.Int.Description)
unique(st_WAOR$Other.Human.Int.Type) # NA
unique(st_WAOR$Condition.at.Examination) # 4 dead categories, plus Condition Unknown & Alive
unique(st_WAOR$Necropsied) # N, Y, NA, ""

# fixing WA-OR dataset
st_WAOR$County=gsub("Jefferson County","Jefferson",st_WAOR$County) # fixing county issue
st_WAOR$Common.Name=gsub("Whale, ","",st_WAOR$Common.Name) # removing "whale," from name

# checking CA post-2006 dataset
# notes: includes info for Fishery.Type & Other.Human.Int.Type
st_CA_post2006$Observation.Date = as.Date(st_CA_post2006$Observation.Date,format="%m/%d/%y") # format date
range(st_CA_post2006$Observation.Date) # 2006 - 2019
unique(st_CA_post2006$Common.Name) # 12 categories incl. multiple unknown/unidentified
unique(st_CA_post2006$State) # CA
unique(st_CA_post2006$County) # 18 listed
unique(st_CA_post2006$Findings.of.Human.Interaction) # CBD, Y, N
unique(st_CA_post2006$Boat.Collision) # N, Y, C, ""
unique(st_CA_post2006$Shot) # N, C, ""
unique(st_CA_post2006$Fishery.Interaction) # N, Y, C, ""
unique(st_CA_post2006$Fishery.Type) # 6 categories incl. "" and Unknown Fishery
unique(st_CA_post2006$Other.Human.Interaction) # N, Y, ""
unique(st_CA_post2006$Other.Human.Int.Description)
unique(st_CA_post2006$Other.Human.Int.Type) # Gaffing/Spearing/Harpooning (n=1), Marine Debris Entanglement (n=2), ""
unique(st_CA_post2006$Condition.at.Examination) # 4 dead categories, plus Condition Unknown, Alive, ""
unique(st_CA_post2006$Necropsied) # N, Y, NA

# fixing CA post-2006 dataset
st_CA_post2006$Common.Name=gsub("Whale, ","",st_CA_post2006$Common.Name) # removing "whale," from name

# combining WA-OR and CA post-2006
# notes: only CA data has Fishery.Type and Other.Human.Int.Type
st = rbind(st_WAOR[,cols_ofinterest],st_CA_post2006[,cols_ofinterest]) # just columns of interest
st = st[st$Observation.Date<"2020-01-01",] # trimming to end at 2019, since WA-OR has 2020 data but CA doesn't
st = st[st$Observation.Date>"2006-01-01",] # trimming to start at 2006 for now, since pre-2006 CA data not ready
st = st[st$Common.Name!="Bryde's",] # trimming to species we want
st$Year = format(as.Date(st$Observation.Date,format="%Y-%m-%d"),"%Y")
st$Common.Name = gsub("UNSPECIFIED BALEEN WHALE|UNKNOWN MYSTICETE|unidentified baleen|unidentified mysticete|UNIDENTIFIED WHALE",
                      "unknown",st$Common.Name) # combining unknowns
st$Condition.at.Examination = gsub("Decomposition","Decomp.",st$Condition.at.Examination)
st$Common.Name = capFirst(st$Common.Name)
st$Common.Name = gsub("Killer","Orca",st$Common.Name)

# deciding how to narrow to dead only
# were any necropsied individuals not dead at examination?
st[st$Condition.at.Examination=="Alive"&st$Necropsied=="Y",] # 1 individual, 17 NAs??
nrow(st[st$Condition.at.Examination%in%c("Advanced Decomp.","Fresh Dead","Moderate Decomp.",
                                         "Mummified/Skeletal"),]) # 503 dead at examination
nrow(st[st$Necropsied=="Y",]) # 288 necropsied
st$Vital.Status = st$Condition.at.Examination
st$Vital.Status[st$Vital.Status%in%c("Advanced Decomp.","Fresh Dead","Moderate Decomp.",
                                     "Mummified/Skeletal")] <- "Dead"
st$Vital.Status = gsub("Condition Unknown","Unknown",st$Vital.Status)
st$Vital.Status[st$Vital.Status==""] <- "Unknown"

# filling blanks in human interaction columns
st$Boat.Collision[st$Boat.Collision==""] <- "C"
st$Shot[st$Shot==""] <- "C"
st$Fishery.Interaction[st$Fishery.Interaction==""] <- "C"
st$Other.Human.Interaction[st$Other.Human.Interaction==""] <- "C"

# subsetting data
st2010 = st[st$Year>=2010,] # dataset of 2010-present only
st2010.gr = st2010[st2010$Common.Name=="Gray",] # gray whales 2010-present
st2010.hu = st2010[st2010$Common.Name=="Humpback",] # humpback whales 2010-present

# adding column to sort counties geographically
counties.st = data.frame(County = c("Whatcom","San Juan","Skagit","Island","Clallam","Snohomish",
                                    "Jefferson","Kitsap","King","Mason","Grays Harbor","Pierce",
                                    "Thurston","Pacific","Clatsop","Tillamook","Lincoln","Lane",
                                    "Douglas","Coos","Curry","Del Norte","Humboldt","Mendocino",
                                    "Sonoma","Sacramento","Marin","Contra Costa","Alameda",
                                    "San Francisco","San Mateo","Santa Cruz","Monterey",
                                    "San Luis Obispo","Santa Barbara","Ventura","Los Angeles",
                                    "Orange","San Diego"),
                         Sort = 1:39)
st = merge(st, counties.st)
counties2010.st = data.frame(County = as.character(
  counties.st[counties.st$County%in%st2010$County,"County"]),
                          Sort = 1:length(unique(st2010$County)))
st2010 = merge(st2010, counties2010.st)


###### graphs
setwd(wd_graphs)

int.labs = c("Ship strike","Shot","Fishery","Other")
names(int.labs) = c("Boat.Collision","Shot","Fishery.Interaction","Other.Human.Interaction")

# strandings by year (color by species)
st.sp.year =
  st %>%
  group_by(Common.Name,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.sp.year, aes(x = Year, y = n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("Year") +
  ylab("Total strandings") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))
dev.copy(dev=png,file="Strand_year_species_allyrs.png",wid=w2,hei=h,
         uni="in",res=450)
dev.off()

# strandings by year (color by alive/dead)
st.vs.year =
  st %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.vs.year, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Spectral") +
  xlab("Year") +
  ylab("Total strandings") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))

# entangled strandings by year (color by alive/dead) ***NEW***
st.vs.year.ent =
  st[st$Fishery.Interaction=="Y",] %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.vs.year.ent, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Set3") +
  xlab("Year") +
  ylab("Strandings - Fishery Int.") +
  ylim(0,18) +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))
dev.copy(dev=png,file="Strand_year_FisheryInt_post2010.png",wid=w2,hei=h,
         uni="in",res=450)
dev.off()

# ship strike strandings by year (color by alive/dead) ***NEW***
st.vs.year.ship =
  st[st$Boat.Collision=="Y",] %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.vs.year.ship, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Set3") +
  xlab("Year") +
  ylab("Strandings - Ship Strike") +
  ylim(0,18) +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))
dev.copy(dev=png,file="Strand_year_ShipStrike_post2010.png",wid=w2,hei=h,
         uni="in",res=450)
dev.off()

# gray whale strandings by year (color by alive/dead)
st.vs.year.gr =
  st[st$Common.Name=="Gray",] %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.vs.year.gr, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Pastel2") +
  xlab("Year") +
  ylab("Gray whale strandings") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))

# humpback whale strandings by year (color by alive/dead)
st.vs.year.hu =
  st[st$Common.Name=="Humpback",] %>%
  group_by(Vital.Status,Year) %>%
  tally()

quartz(height=h,width=w2)
ggplot(st.vs.year.hu, aes(x = Year, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Pastel2") +
  xlab("Year") +
  ylab("Humpback whale strandings") +
  theme_classic() +
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=90, vjust=0.5))

# strandings by county (color by species) 
st.sp.county =
  st2010 %>%
  group_by(Common.Name,Sort) %>%
  tally()

quartz(height=h,width=w3)
ggplot(st.sp.county, aes(x = Sort, y = n, fill=Common.Name)) + 
  geom_col() +
  colScale +
  xlab("County") +
  ylab("Strandings 2010-present") +
  scale_x_discrete(limits=counties2010.st$County) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
dev.copy(dev=png,file="Strand_county_species_post2010.png",wid=w3,hei=h,
         uni="in",res=450)
dev.off()

# gray whale strandings by county (color by alive/dead)
st.sp.county.gr =
  st2010[st2010$Common.Name=="Gray",] %>%
  group_by(Vital.Status,Sort) %>%
  tally()

quartz(height=h,width=w3)
ggplot(st.sp.county.gr, aes(x = Sort, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Pastel2") +
  xlab("County") +
  ylab("Gray whale strandings") +
  scale_x_discrete(limits=counties2010.st$County) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# humpback whale strandings by county (color by alive/dead)
st.sp.county.hu =
  st2010[st2010$Common.Name=="Humpback",] %>%
  group_by(Vital.Status,Sort) %>%
  tally()

quartz(height=h,width=w3)
ggplot(st.sp.county.hu, aes(x = Sort, y = n, fill=Vital.Status)) + 
  geom_col() +
  scale_fill_brewer(palette="Pastel2") +
  xlab("County") +
  ylab("Humpback whale strandings") +
  scale_x_discrete(limits=counties2010.st$County) +               # x-axis needs to be fixed
  theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# human interaction findings -- gray -- color by evidence (C/N/Y)
dat = data.frame(Common.Name=rep("Gray",12),
                 Stat=rep(c("Boat.Collision","Shot","Fishery.Interaction",
                            "Other.Human.Interaction"),each=3),
                 Value=rep(c("Y","N","C"),4))

hum.int.gr = st[st$Common.Name=="Gray",c("Boat.Collision","Shot","Fishery.Interaction",
                                         "Other.Human.Interaction")]
hum.int.gr.long = hum.int.gr %>%
  gather("Stat","Value")

tab.gr =
  hum.int.gr.long %>%
  group_by(Stat,Value) %>%
  tally()

tab.gr = merge(dat,tab.gr,all.x=T)

quartz(height=h,width=w)
ggplot(tab.gr, aes(x = Stat, y = n, fill = Value)) +
  geom_col(position=position_dodge()) +
  xlab("Human interaction") +
  ylab("Gray whale strandings") +
  scale_fill_brewer(palette="Set3",direction=-1) +
  scale_x_discrete(labels=c("Ship strike","Fishery","Other","Shot")) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1),legend.title=element_blank())
dev.copy(dev=png,file="Strand_HumInt_gray_post2010.png",wid=w,hei=h,
         uni="in",res=450)
dev.off()

# human interaction findings -- gray -- color by vital status
hum.int.gr = data.frame(Common.Name=rep("Gray",36),
                        Int=rep(c("Boat.Collision","Shot","Fishery.Interaction",
                                   "Other.Human.Interaction"),each=9),
                        Evidence=factor(rep(rep(c("Y","N","C"),each=3),4),levels=c("Y","N","C")),
                        Vital.Status=rep(c("Alive","Dead","Unknown"),12))

for(i in unique(hum.int.gr$Int)){
  for(e in unique(hum.int.gr$Evidence)){
    for(v in unique(hum.int.gr$Vital.Status)){
      hum.int.gr$n[hum.int.gr$Int==i&hum.int.gr$Evidence==e&hum.int.gr$Vital.Status==v] <-
        nrow(st2010.gr[which(st2010.gr[,i]==e&st2010.gr[,"Vital.Status"]==v),])
    }
  }
}

quartz(height=h2,width=w2)
ggplot(hum.int.gr, aes(x=Evidence, y=n, fill=Vital.Status)) +
  geom_col() +
  ylab("Gray whale strandings") +
  facet_wrap(~Int, labeller=labeller(Int=int.labs)) +
  scale_fill_brewer(palette="Set3") +
  theme_light()
dev.copy(dev=png,file="Strand_HumInt_gray_vs_post2010.png",wid=w2,hei=h2,
         uni="in",res=450)
dev.off()

# human interaction findings -- humpback -- color by vital status
hum.int.hu = data.frame(Common.Name=rep("Humpback",36),
                        Int=rep(c("Boat.Collision","Shot","Fishery.Interaction",
                                  "Other.Human.Interaction"),each=9),
                        Evidence=factor(rep(rep(c("Y","N","C"),each=3),4),levels=c("Y","N","C")),
                        Vital.Status=rep(c("Alive","Dead","Unknown"),12))

for(i in unique(hum.int.hu$Int)){
  for(e in unique(hum.int.hu$Evidence)){
    for(v in unique(hum.int.hu$Vital.Status)){
      hum.int.hu$n[hum.int.hu$Int==i&hum.int.hu$Evidence==e&hum.int.hu$Vital.Status==v] <-
        nrow(st2010.hu[which(st2010.hu[,i]==e&st2010.hu[,"Vital.Status"]==v),])
    }
  }
}

quartz(height=h2,width=w2)
ggplot(hum.int.hu, aes(x=Evidence, y=n, fill=Vital.Status)) +
  geom_col() +
  ylab("Humpback whale strandings") +
  facet_wrap(~Int, labeller=labeller(Int=int.labs)) +
  scale_fill_brewer(palette="Set3") +
  theme_light()
dev.copy(dev=png,file="Strand_HumInt_humpback_vs_post2010.png",wid=w2,hei=h2,
         uni="in",res=450)
dev.off()




 



