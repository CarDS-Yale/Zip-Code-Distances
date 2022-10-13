#packages
library(datasets)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, 
               lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, readr, 
               modelr, mice, gmodels,zipcodeR,readxl)

#Set Directory, Import Data
setwd("~/Research/Afib Ablation")
df_af<-read.csv("df_af.csv") #zip codes with AF ablation
zip<-read_xlsx("uszips.xlsx") #all US zip codes

df_af<-df_af%>%
  filter(Rndrng_Prvdr_State_Abrvtn!="PR")%>% #remove PR
  select(-X)%>%
  rename(City=Rndrng_Prvdr_City,State=Rndrng_Prvdr_State_Abrvtn)

zip<-zip %>%  #remove territories, HI, and AK
  filter(state_id!='PR')%>%
  filter(state_id!="HI")%>%
  filter(state_id!="MP")%>%
  filter(state_id!="VI")%>%
  filter(state_id!="GU")%>%
  filter(state_id!="AK")

#NE: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont,
#New Jersey, New York and Pennsylvania

  #filter both files for just NE states (and bordering states)
zip_NE<-zip%>%
  filter(state_id=="CT"|state_id=='ME'|state_id=='MA'|state_id=='NH'|
         state_id=='RI'|state_id=='VT'|state_id=='NJ'|state_id=='NY'|state_id=='PA'|
         state_id=='OH'|state_id=='WV'|state_id=='RI'|state_id=='DE'|
         state_id=='MD')

df_af_NE<-df_af%>%
  filter(State=="CT"|State=='ME'|State=='MA'|State=='NH'|
           State=='RI'|State=='VT'|State=='NJ'|State=='NY'|State=='PA'|
           State=='OH'|State=='WV'|State=='RI'|State=='DE'|
           State=='MD')

  #extract zip codes from each df, filter AF ablation zip codes for
  #only unique zip codes
zip_af = data.frame(zip = as.character(unique(df_af_NE$Rndrng_Prvdr_Zip5)))
zips = data.frame(zip = as.character(zip_NE$zip))

  #replace leading zeros for zip codes <5 digits in length
  #since R automatically drops leading zeros
zip_char = data.frame(zip = apply(zips, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

zip_af_c = data.frame(zip = apply(zip_af, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

  #function to calculate the zip distance using zip_distance
  #from the R package and return the minimum distance
test = apply(zip_char, 1, function(x){
  dist = zip_distance(rep(x, length(zip_af_c$zip)),
               zip_af_c$zip,
               lonlat = TRUE,
               units = "miles")
  out = dist[which(dist$distance == min(dist$distance, na.rm = TRUE)),]
  return(out)
  })

  #create unique df for just NE region and export to avoid 
  #needing to repeat procedure
dists_NE = do.call(rbind, test)
write.csv(dists_NE,file='dists_NE.csv')

#Midwest Region
#Illinois, Indiana, Michigan, Ohio and Wisconsin
#Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota and South Dakota


zip_MW<-zip%>%
  filter(state_id=="IL"|state_id=='IN'|state_id=='MI'|state_id=='OH'|
           state_id=='WI'|state_id=='IA'|state_id=='KS'|state_id=='MN'|
           state_id=='MO'|state_id=='NE'|state_id=='ND'|state_id=='SD'|
           state_id=='MT'|state_id=='WY'|state_id=='CO'|KSstate_id=='OK'|
           state_id=='AR'|state_id=='TN'|state_id=='KY'|state_id=='WV'|
           state_id=='PA')

df_af_MW<-df_af%>%
  filter(State=="IL"|State=='IN'|State=='MI'|State=='OH'|
           State=='WI'|State=='IA'|State=='KS'|State=='MN'|
           State=='MO'|State=='NE'|State=='ND'|State=='SD'|
           State=='MT'|State=='WY'|State=='CO'|KSState=='OK'|
           State=='AR'|State=='TN'|State=='KY'|State=='WV'|
           State=='PA')

zip_af = data.frame(zip = as.character(unique(df_af_MW$Rndrng_Prvdr_Zip5)))
zips = data.frame(zip = as.character(zip_MW$zip))

zip_char = data.frame(zip = apply(zips, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

zip_af_c = data.frame(zip = apply(zip_af, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

test = apply(zip_char, 1, function(x){
  dist = zip_distance(rep(x, length(zip_af_c$zip)),
                      zip_af_c$zip,
                      lonlat = TRUE,
                      units = "miles")
  out = dist[which(dist$distance == min(dist$distance, na.rm = TRUE)),]
  return(out)
})

dists_MW = do.call(rbind, test)
write.csv(dists_MW,file='dists_MW.csv')

#South Region
#Delaware, District of Columbia, Florida, Georgia, Maryland, North Carolina, 
#South Carolina, Virginia and West Virginia
#Alabama, Kentucky, Mississippi and Tennessee
#Arkansas, Louisiana, Oklahoma and Texas

zip_S<-zip%>%
  filter(state_id=="DE"|state_id=='DC'|state_id=='FL'|state_id=='MD'|
           state_id=='NC'|state_id=='SC'|state_id=='VA'|state_id=='WV'
         |state_id=='AL'|state_id=='KY'|state_id=='MS'|state_id=='TN'
         |state_id=='AR'|state_id=='LA'|state_id=='OK'|state_id=='TX'|
          state_id=='NM'|state_id=='CO'|state_id=='AR'|KSstate_id=='MO'|
          state_id=='IL'|state_id=='IN'|state_id=='OH'|state_id=='PA'|
          state_id=='NJ')

df_af_S<-df_af%>%
  filter(State=="DE"|State=='DC'|State=='FL'|State=='MD'|
           State=='NC'|State=='SC'|State=='VA'|State=='WV'
         |State=='AL'|State=='KY'|State=='MS'|State=='TN'
         |State=='AR'|State=='LA'|State=='OK'|State=='TX'|
           State=='NM'|State=='CO'|State=='AR'|KSState=='MO'|
           State=='IL'|State=='IN'|State=='OH'|State=='PA'|
           State=='NJ')

zip_af = data.frame(zip = as.character(unique(df_af_S$Rndrng_Prvdr_Zip5)))
zips = data.frame(zip = as.character(zip_S$zip))

zip_char = data.frame(zip = apply(zips, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

zip_af_c = data.frame(zip = apply(zip_af, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

test = apply(zip_char, 1, function(x){
  dist = zip_distance(rep(x, length(zip_af_c$zip)),
                      zip_af_c$zip,
                      lonlat = TRUE,
                      units = "miles")
  out = dist[which(dist$distance == min(dist$distance, na.rm = TRUE)),]
  return(out)
})

dists_S = do.call(rbind, test)
write.csv(dists_S,file='dists_S.csv')

#West Region
#Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah and Wyoming
#California, Oregon and Washington

zip_W<-zip%>%
  filter(state_id=="AZ"|state_id=='CO'|state_id=='ID'|state_id=='MT'|
           state_id=='NV'|state_id=='NM'|state_id=='UT'|state_id=='WY'
         |state_id=='CA'|state_id=='OR'|state_id=='WA'|
          state_id=='TX'|state_id=='OK'|state_id=='KS'|KSstate_id=='NE'|
          state_id=='SD'|state_id=='ND')

df_af_W<-df_af%>%
  filter(State=="AZ"|State=='CO'|State=='ID'|State=='MT'|
           State=='NV'|State=='NM'|State=='UT'|State=='WY'
         |State=='CA'|State=='OR'|State=='WA'|
           State=='TX'|State=='OK'|State=='KS'|State=='NE'|
           State=='SD'|State=='ND')

zip_af = data.frame(zip = as.character(unique(df_af_W$Rndrng_Prvdr_Zip5)))
zips = data.frame(zip = as.character(zip_W$zip))

zip_char = data.frame(zip = apply(zips, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

zip_af_c = data.frame(zip = apply(zip_af, 1, function(x){
  out = paste(strrep(0, 5-nchar(x)), x, sep = "")
  return(out)
}))

test = apply(zip_char, 1, function(x){
  dist = zip_distance(rep(x, length(zip_af_c$zip)),
                      zip_af_c$zip,
                      lonlat = TRUE,
                      units = "miles")
  out = dist[which(dist$distance == min(dist$distance, na.rm = TRUE)),]
  return(out)
})

dists_W = do.call(rbind, test)
write.csv(dists_W,file='dists_W.csv')

#distance analysis

  #import files
setwd("~/Research/Afib Ablation")
dists_MW<-read.csv("dists_MW.csv")
dists_NE<-read.csv("dists_NE.csv")
dists_S<-read.csv("dists_S.csv")
dists_W<-read.csv("dists_W.csv")

dists_MW['region']='Midwest'
dists_NE['region']='Northeast'
dists_S['region']='South'
dists_W['region']='West'

dists <- rbind(dists_MW,dists_W,dists_NE,dists_S)
dists<-dists %>%
  select(-X) 

boxplot<-dists %>%
  ggplot(aes(x=region,y=distance))+
  geom_boxplot()+
  labs(x="",y="")+
  theme_classic()
boxplot

maximum <-dists %>%
  group_by(region)%>%
  summarize(max=max(distance))
maximum



