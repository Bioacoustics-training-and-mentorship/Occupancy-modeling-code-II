# Full Sierra Multiseason Model 

### Import data, make basic identifiers ###
{

  # date range: 80:128], # 18-May : 5-July (49 days)

  ## survey effort ##

  # 2021 effort
  {
    effort.21.full=read.csv('data/Survey Effort/2021_Effort_perUnit_0400-0859_1800-1959_split0.csv')
    for(i in 1:dim(effort.21.full)[1]){
      if(nchar(effort.21.full$Cell_U)[i]==7){
        effort.21.full$Cell_U[i] = paste("C0", substr(effort.21.full$Cell_U[i],2,7), sep='')
      } else {
        next
      }
    }

    effort.21=data.frame(effort.21.full[,
                                        # NOTE: need to manually update the dimensions to tmp in the main loop
                                        c(1,80:128)]) # 18-May : 5-July (49 days)
    effort.21=effort.21[order(effort.21$Cell_U),]
    n.days.21=dim(effort.21)[2]-1
    
    # # how much late-season survey effort? #
    # active.units=rep(NA, n.days)
    # for(d in 1:n.days){
    #   active.units[d]=(n.arus-sum(is.na(detections[1,d,])) )
    # }
    # plot(active.units, type='l', lwd=4,
    #      xlab='Days after June 1', ylab='Number of active units')
    
  }
  # 2022 effort
  {  
    effort.22.full=read.csv('data/Survey Effort/2022_Effort_perUnit_0400-0859_1800-1959_split0.csv')
    for(i in 1:dim(effort.22.full)[1]){
      if(nchar(effort.22.full$Cell_U)[i]==7){
        effort.22.full$Cell_U[i] = paste("C0", substr(effort.22.full$Cell_U[i],2,7), sep='')
      } else {
        next
      }
    }
    effort.22=data.frame(effort.22.full[,
                                        # NOTE: need to manually update the dimensions to tmp in the main loop
                                        c(1,80:128)]) # 18-May : 5-July (49 days)       
    effort.22=effort.22[order(effort.22$Cell_U),]
    n.days.22=dim(effort.22)[2]-1
    
    # # how much late-season survey effort? #
    # active.units=rep(NA, n.days.22)
    # for(d in 1:n.days.22){
    #   active.units[d]=(n.arus-sum(is.na(detections[1,d,])) )
    # }
    # plot(active.units, type='l', lwd=4,
    #      xlab='Days after June 1', ylab='Number of active units')
    
  }
  # 2023 effort
  {  
    effort.23.full=read.csv('data/Survey Effort/2023_Effort_perUnit_400-859_1800-1959_split0.csv')
    for(i in 1:dim(effort.23.full)[1]){
      if(nchar(effort.23.full$Cell_U)[i]==7){
        effort.23.full$Cell_U[i] = paste("C0", substr(effort.23.full$Cell_U[i],2,7), sep='')
      } else {
        next
      }
    }
    effort.23=data.frame(effort.23.full[,
                                        # NOTE: need to manually update the dimensions to tmp in the main loop
                                        c(1,80:128)]) # 18-May : 5-July (49 days)
    effort.23=effort.23[order(effort.23$Cell_U),]
    n.days.23=dim(effort.23)[2]-1
    
    # # how much late-season survey effort? #
    # active.units=rep(NA, n.days.22)
    # for(d in 1:n.days.22){
    #   active.units[d]=(n.arus-sum(is.na(detections[1,d,])) )
    # }
    # plot(active.units, type='l', lwd=4,
    #      xlab='Days after June 1', ylab='Number of active units')
    
  }
  
  # check for duplicated entries - none found
  sum(duplicated(effort.21$Cell_U))
  sum(duplicated(effort.22$Cell_U))
  sum(duplicated(effort.23$Cell_U))
  
  # total annual survey effort
  sum(as.matrix(effort.21[,2:49])) #350483
  sum(as.matrix(effort.22[,2:49])) #310495
  sum(as.matrix(effort.23[,2:49])) #239668
  
  ## thresholds ##
  thresholds=read.csv('data/Thresholds_2021_20230309.csv')

  
  ## Habitat Data ##
  # habitat.full=read.csv('./Updated_Habitat_data_2023_03_28/ARU_390m.csv')
  # habitat=habitat.full[habitat.full$cell_unit %in% rownames(effort) & 
  #                        habitat.full$cell_unit %in% target.files$ID,]
  # habitat=habitat[order(habitat$cell_unit),]
  # rownames(habitat) = habitat$cell_unit

}  


## read in max_score_summary file for each year ##
{
  encounters.21=read.csv('data/MultiSeason - from MaxScore/Hermit_Warbler_Gt0.578_2021_max_score_summary.csv')
  encounters.22=read.csv('data/MultiSeason - from MaxScore/Hermit_Warbler_Gt0.578_2022_max_score_summary.csv')
  encounters.23=read.csv('data/MultiSeason - from MaxScore/Hermit_Warbler_Gt0.578_2023_max_score_summary.csv')
  
  summary(nchar(encounters.21$Cell_Unit))
  
  # need to add leading zeros for encounters$e2021
  for(i in 1:dim(encounters.21)[1]){
    if(nchar(encounters.21$Cell_Unit)[i]==7){
      encounters.21$Cell_Unit[i] = paste("C0", substr(encounters.21$Cell_Unit[i],2,7), sep='')
    } else {
      next
    }
  }
  
  names(encounters.23)
  
}


## build encounter & effort history ##
all.arus=sort(unique(c( effort.21$Cell_U[effort.21$Cell_U %in% encounters.21$Cell_Unit],
                        effort.22$Cell_U[effort.22$Cell_U %in% encounters.22$Cell_Unit],
                        effort.23$Cell_U[effort.23$Cell_U %in% encounters.23$Cell_Unit])))

# compile daily encounter history
encounters=merge(encounters.21[,c(1,80:128)],
                    merge(encounters.22[,c(1,80:128)], encounters.23[,c(1,80:128)], by= 'Cell_Unit',
                          all.x=T, all.y=T, sort=T),
                    by= 'Cell_Unit',
                    all.x=T, all.y=T, sort=T)
encounters[encounters=="."] <- NA
encounters=as.data.frame(lapply(encounters[,2:148], 
                         as.numeric), row.names = all.arus)

dim(encounters)#.21[,c(1,80:128)])

# compile daily effort history
effort=merge(effort.21,
                 merge(effort.22, effort.23, by= 'Cell_U',
                       all.x=T, all.y=T, sort=T),
                 by= 'Cell_U',
                 all.x=T, all.y=T, sort=T)
effort=as.data.frame(effort[,2:148],
                     row.names = effort$Cell_U)


### Model settings and structure ###
{
  min.score="95" # choose "90", "95", or "99"
  min.det.days=3 # if not using an FP model, set a filter for detection days
  
  det.probs=F # if TRUE, reports probabilities, not binary observations. NOTE - currently only works for 90
  round.up.value=0.9995 # if det.probs=T, when is a detection rounded to "1"?
  
  ssp.length=7 # 6 days with a 48-day period was computationally too much
  
  years=3 # seasons of data
  
  

  ## Build empty encounter & effort histories (ssp level)
  if(n.days.21==n.days.22 & n.days.22==n.days.23){
    n.days = n.days.21
  } else {
    print('ERROR: seasons differ in length')
  }
  sampling.start=seq(from=1, to=n.days*years, by=ssp.length)
  sampling.stop=seq(from=ssp.length, to=n.days*years, by=ssp.length)
  sample.intervals=length(sampling.start)
  n.ssp=ceiling(n.days/ssp.length)
  dets=matrix(NA, nrow=length(all.arus), ncol=n.ssp*years)
  eff=matrix(NA, nrow=length(all.arus), ncol=n.ssp*years)
}

### Main loop ###

for(a in 1:length(all.arus)){
  for(s in 1:sample.intervals){
    # if(sum(is.na(effort[a, sampling.start[s]:sampling.stop[s]]))==ssp.length){
    #   eff[a,s] = NA ##### 'unmarked' might not allow 'NA' covariate values - if so, drop this
    # } else {
      eff[a,s]=sum(effort[a, sampling.start[s]:sampling.stop[s]], na.rm=T)
      if(eff[a,s]>0){
        if( max(encounters[a, sampling.start[s]:sampling.stop[s]], na.rm=T) 
            >= thresholds$cutoff95.r_conf[thresholds$species == 'Evening Grosbeak']){
          dets[a,s]=1
        } else {
          dets[a,s]=0
        }
      }
    #}
  }
}

head(eff)

### Fit models ###
library(unmarked)

str(eff)
evgrUMF = unmarkedMultFrame(dets, 
                            obsCovs = list(occasion = as.data.frame(eff)),
                            numPrimary = 3)
summary(evgrUMF)
m0=colext(psiformula = ~1, 
          gammaformula = ~1, 
          epsilonformula = ~1, 
          pformula = ~1,
          data = evgrUMF)
m1=colext(psiformula = ~1, 
          gammaformula = ~1, 
          epsilonformula = ~1, 
          pformula = ~occasion,
          data = evgrUMF)
summary(m0)
summary(m1)
plogis(1.39) # initial occ = 0.8
plogis(-0.191) # pr(col) = 0.45
plogis(-1.43) # pr(ext) = 0.19

yr1 = plogis(1.39)
yr2 = yr1 + (1-yr1)*plogis(-0.191) - yr1*plogis(-1.43)
yr3 = yr2 + (1-yr2)*plogis(-0.191) - yr2*plogis(-1.43)
c(yr1,yr2,yr3)
