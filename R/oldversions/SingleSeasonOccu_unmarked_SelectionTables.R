## Processing BirdNET outputs: selection table format ##
# Connor Wood - BirdNET Ecology Lead - cmw289@cornell.edu

library(readxl,stringr)

# select folder containing all desired selection tables (subfolders ok)
files=data.frame(File=list.files(path='data/SingleSeason - from pre-made inputs',
                                 recursive=T,pattern='txt$',full.names = T))

## compile file list, effort
{
  files$Site=substr(files$File,75,79)
  files$Date=as.Date( paste(
    substr(files$File,81,84),
    substr(files$File,85,86),
    substr(files$File,87,88), sep='/'))
  files$Hour=as.numeric(substr(files$File,90,91))
  
  all.sites=sort(unique(files$Site))
  n.sites=length(all.sites)
  all.dates=seq.Date( from=min(files$Date), to=max(files$Date), by='day')
  n.dates=length(all.dates)
  
  effort=matrix(data=NA, # defaults to no survey effort
                nrow=n.sites, ncol=n.dates, dimnames=list(all.sites))
  
  # populate daily effort file based on BirdNET outputs
  for(s in 1:n.sites){
    for(d in 1:n.dates){
      if(dim(files[files$Site==all.sites[s] & files$Date==all.dates[d],])[1] > 0){
        effort[s,d]=dim(files[files$Site==all.sites[s] & files$Date==all.dates[d],])[1]
      }
    }
  }
}

## species thresholds ##
{
  min.conf_ASCI = .613 # minimum score: Ashy Cisticola; pr(tp) >= 0.99 (confidence scale)
  min.conf_NUWO = .25 # minimum score: Nubian Woodpecker; all correct
  min.conf_GRWH = .896 # minimum score: Green Woodhoopoe; pr(tp) >= 0.99 (confidence scale)
  min.conf_GHKI = .532 # minimum score: Gray-headed Kingfisher; pr(tp) >= 0.99 (confidence scale)
  min.conf_AFPI = .609 # minimum score: African Pipit; pr(tp) >= 0.99 (confidence scale)
}

### Compile bird observations, create encounter histories

# set the duration of the secondary sampling period
ssp.length=4 # days
n.ssp=ceiling(n.dates/ssp.length)


for(f in 1:length(files$File)){
  if(f==1){
    dt_ASCI=NA
    dt_NUWO=NA
    dt_GRWH=NA
    dt_GHKF=NA
    dt_AFPI=NA
  }
  
  tmp=read.table(files$File[f], sep="\t", header=T, quote="", fill=F)
  # add location ID, Date, Time based on file path folder hierarchy
  if(dim(tmp)[1] != 0){
    tmp$Site=substr(files$File[f],75,79)
    tmp$Date=as.Date( paste(
      substr(files$File[f],81,84),
      substr(files$File[f],85,86),
      substr(files$File[f],87,88), sep='/'))
    tmp$Hour=as.numeric(substr(files$File[f],90,91))
    tmp$Min=as.numeric(substr(files$File[f],92,93))
  }
  
  # compile bird data
  if( dim(tmp[tmp$Common.Name=="Ashy Cisticola" & tmp$Confidence>=min.conf_ASCI,])[1] > 0){
    dt_ASCI=rbind(dt_ASCI, tmp[tmp$Common.Name=="Ashy Cisticola" & tmp$Confidence>=min.conf_ASCI,])
  } 
  if( dim(tmp[tmp$Common.Name=="Nubian Woodpecker" & tmp$Confidence>=min.conf_NUWO,])[1] > 0){
    dt_NUWO=rbind(dt_NUWO, tmp[tmp$Common.Name=="Nubian Woodpecker" & tmp$Confidence>=min.conf_NUWO,])
  } 
  if( dim(tmp[tmp$Common.Name=="Green Woodhoopoe" & tmp$Confidence>=min.conf_GRWH,])[1] > 0){
    dt_GRWH=rbind(dt_GRWH, tmp[tmp$Common.Name=="Green Woodhoopoe" & tmp$Confidence>=min.conf_GRWH,])
  } 
  if( dim(tmp[tmp$Common.Name=="Gray-headed Kingfisher" & tmp$Confidence>=min.conf_GHKI,])[1] > 0){
    dt_GHKF=rbind(dt_GHKF, tmp[tmp$Common.Name=="Gray-headed Kingfisher" & tmp$Confidence>=min.conf_GHKI,])
  } 
  if( dim(tmp[tmp$Common.Name=="African Pipit" & tmp$Confidence>=min.conf_AFPI,])[1] > 0){
    dt_AFPI=rbind(dt_AFPI, tmp[tmp$Common.Name=="African Pipit" & tmp$Confidence>=min.conf_AFPI,])
  } 
  
  
  print(paste(f,'of',length(files$File),sep=' ')) # displays progress
  
  if(f==length(files$File)){
    
    print('Converting observations to encounter histories')
    
    # remove empty first row from bird data
    dt_ASCI=dt_ASCI[-1,]
    dt_NUWO=dt_NUWO[-1,]
    dt_GRWH=dt_GRWH[-1,]
    dt_GHKF=dt_GHKF[-1,]
    dt_AFPI=dt_AFPI[-1,]
    
    # create effort files for encounter histories (and as observation covariates)
    effort_hrs=matrix(data=NA, nrow=n.sites, ncol=n.ssp, 
                      dimnames=list(all.sites))#,
    # c('hours1','hours2','hours3','hours4','hours5',
    #   'hours6','hours7','hours8','hours9','hours10')))
    effort_days=matrix(data=NA, nrow=n.sites, ncol=n.ssp, 
                       dimnames=list(all.sites))#,
    # c('days1','days2','days3','days4','days5',
    #   'days6','days7','days8','days9','days10')))
    
    ssp.start=seq(1,n.dates,ssp.length)
    ssp.stop=seq(ssp.length,n.dates,ssp.length)
    if(length(ssp.stop)<length(ssp.start)){
      ssp.stop=c(ssp.stop,n.dates)
    }
    
    for(s in 1:n.sites){
      for(p in 1:n.ssp){
        effort_hrs[s,p]=sum(effort[s,ssp.start[p]:ssp.stop[p]], na.rm=T)
        effort_days[s,p]=sum(!is.na(effort[s,ssp.start[p]:ssp.stop[p]]))
      }
    }
    
    # create bird encounter histories
    eh_ASCI=matrix(data=NA, nrow=n.sites, ncol=ceiling(n.dates/ssp.length))
    eh_NUWO=matrix(data=NA, nrow=n.sites, ncol=ceiling(n.dates/ssp.length))
    eh_GRWH=matrix(data=NA, nrow=n.sites, ncol=ceiling(n.dates/ssp.length))
    eh_GHKF=matrix(data=NA, nrow=n.sites, ncol=ceiling(n.dates/ssp.length))
    eh_AFPI=matrix(data=NA, nrow=n.sites, ncol=ceiling(n.dates/ssp.length))
    
    for(s in 1:n.sites){
      for(p in 1:n.ssp){
        if(effort_days[s,p]>0){
          
          if(dim(dt_ASCI[dt_ASCI$Site==rownames(effort_days)[s] & 
                         dt_ASCI$Date %in% all.dates[ssp.start[p]:ssp.stop[p]], ])[1]==0){
            eh_ASCI[s,p]=0
          } else {
            # obs-day filter here
            eh_ASCI[s,p]=1
          }
          
          # NUWO
          if(dim(dt_NUWO[dt_NUWO$Site==rownames(effort_days)[s] & 
                         dt_NUWO$Date %in% all.dates[ssp.start[p]:ssp.stop[p]], ])[1]==0){
            eh_NUWO[s,p]=0
          } else {
            # obs-day filter here
            eh_NUWO[s,p]=1
          }
          
          if(dim(dt_GRWH[dt_GRWH$Site==rownames(effort_days)[s] & 
                         dt_GRWH$Date %in% all.dates[ssp.start[p]:ssp.stop[p]], ])[1]==0){
            eh_GRWH[s,p]=0
          } else {
            # obs-day filter here
            eh_GRWH[s,p]=1
          }
          
          if(dim(dt_GHKF[dt_GHKF$Site==rownames(effort_days)[s] & 
                         dt_GHKF$Date %in% all.dates[ssp.start[p]:ssp.stop[p]], ])[1]==0){
            eh_GHKF[s,p]=0
          } else {
            # obs-day filter here
            eh_GHKF[s,p]=1
          }
          
          if(dim(dt_AFPI[dt_AFPI$Site==rownames(effort_days)[s] & 
                         dt_AFPI$Date %in% all.dates[ssp.start[p]:ssp.stop[p]], ])[1]==0){
            eh_AFPI[s,p]=0
          } else {
            # obs-day filter here
            eh_AFPI[s,p]=1
          }
          
        }
      }
    }
    
  }
}

bird_summary=data.frame(
  species=c('Ashy Cisticola', 'Nubian Woodpecker', 'Green Woodhoopoe', 'Gray-headed Kingfisher', 'African Pipit'),
  occ=c(length(unique(dt_ASCI$Site)), length(unique(dt_NUWO$Site)), 
        length(unique(dt_GRWH$Site)), length(unique(dt_GHKF$Site)), length(unique(dt_AFPI$Site))),
  naive=round( c(length(unique(dt_ASCI$Site)), length(unique(dt_NUWO$Site)), 
                 length(unique(dt_GRWH$Site)), length(unique(dt_GHKF$Site)), length(unique(dt_AFPI$Site)))
               /n.sites, 3)
)
bird_summary

### Export data for Alice
write.csv(data.frame(eh_ASCI,ID=rownames(effort_days)), 'Ashy Cisticola.csv', row.names = F)
write.csv(data.frame(eh_NUWO,ID=rownames(effort_days)),  'Nubian Woodpecker.csv', row.names = F)
write.csv(data.frame(eh_GRWH,ID=rownames(effort_days)), 'Green Woodhoopoe.csv', row.names = F)
write.csv(data.frame(eh_AFPI,ID=rownames(effort_days)), 'African Pipit.csv', row.names = F)

write.csv(data.frame(effort_days,ID=rownames(effort_days)), 'effort_days.csv', row.names = F)
write.csv(data.frame(effort_hrs,ID=rownames(effort_hrs)), 'effort_hrs.csv', row.names = F)

#### Fit some occupancy models! 

library(unmarked)


## Nubian Woodpecker
occ_NUWO <- unmarkedFrameOccu(eh_NUWO, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 

head(effort_days)

NUWO.0 <- occu(~1 ~1, occ_NUWO)
NUWO.1 <- occu(~hours ~1, occ_NUWO)
NUWO.2 <- occu(~days ~1, occ_NUWO)
NUWO.12 <- occu(~hours+days ~1, occ_NUWO)

modSel(fitList(null=NUWO.0, hours=NUWO.1, days=NUWO.2, hours_days=NUWO.12))
summary(NUWO.2)

backTransform(NUWO.0, "det")
backTransform(NUWO.0, "state")
backTransform(NUWO.2, "state")

## Ashy Cisticola 
occ_ASCI <- unmarkedFrameOccu(eh_ASCI, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 

ASCI.0 <- occu( ~1 ~1, occ_ASCI)
ASCI.1 <- occu(~hours ~1, occ_ASCI)
ASCI.2 <- occu(~days ~1, occ_ASCI)
ASCI.12 <- occu(~hours+days ~1, occ_ASCI)

modSel(fitList(null=ASCI.0, hours=ASCI.1, days=ASCI.2, hours_days=ASCI.12))
summary(ASCI.1)

backTransform(ASCI.0, "det")
backTransform(ASCI.0, "state")
backTransform(ASCI.1, "state")

## Green Woodhoopoe
occ_GRWH <- unmarkedFrameOccu(eh_GRWH, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 

GRWH.0 <- occu( ~1 ~1, occ_GRWH)
GRWH.1 <- occu(~hours ~1, occ_GRWH)
GRWH.2 <- occu(~days ~1, occ_GRWH)
GRWH.12 <- occu(~hours+days ~1, occ_GRWH)

modSel(fitList(null=GRWH.0, hours=GRWH.1, days=GRWH.2, hours_days=GRWH.12))
summary(GRWH.1)

backTransform(GRWH.0, "det")
backTransform(GRWH.0, "state")
backTransform(GRWH.1, "state")
backTransform(GRWH.1, "state")@estimate

## Gray-headed Kingfisher - POSSIBLE PROBLEM?
occ_GHKF <- unmarkedFrameOccu(eh_GHKF, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 
GHKF.0 <- occu( ~1 ~1, occ_GHKF)
GHKF.1 <- occu(~hours ~1, occ_GHKF)
GHKF.2 <- occu(~days ~1, occ_GHKF)
GHKF.12 <- occu(~hours+days ~1, occ_GHKF)

modSel(fitList(null=GHKF.0, hours=GHKF.1, days=GHKF.2, hours_days=GHKF.12))
backTransform(GHKF.0, "det")
backTransform(GHKF.0, "state")
backTransform(GHKF.2, "state")

## African Pipit
occ_AFPI <- unmarkedFrameOccu(eh_AFPI, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 
AFPI.0 <- occu( ~1 ~1, occ_AFPI)
AFPI.1 <- occu(~hours ~1, occ_AFPI)
AFPI.2 <- occu(~days ~1, occ_AFPI)
AFPI.12 <- occu(~hours+days ~1, occ_AFPI)

modSel(fitList(null=AFPI.0, hours=AFPI.1, days=AFPI.2, hours_days=AFPI.12))
backTransform(AFPI.0, "det")
backTransform(AFPI.0, "state")


## updating the summary file
bird_summary$est=c(round(backTransform(ASCI.1, "state")@estimate, 3), 
                   round(backTransform(NUWO.2, "state")@estimate, 3), 
                   round(backTransform(GRWH.1, "state")@estimate, 3),
                   NA,
                   round(backTransform(AFPI.0, "state")@estimate, 3))
