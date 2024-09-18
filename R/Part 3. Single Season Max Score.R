# Run occupancy on single season max scores

## Effort
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

effort=as.data.frame(effort.21[,2:50],
                     row.names = effort.21$Cell_U)



## thresholds ##
thresholds=read.csv('data/Thresholds_2021_20230309.csv')


## Habitat Data ##
habitat.full=read.csv('data/2021_ARU_120m.csv')
habitat=habitat.full[habitat.full$cell_unit %in% rownames(effort), ]
habitat=habitat[order(habitat$cell_unit),]
rownames(habitat) = habitat$cell_unit

## Bird data ##
encounters.21=read.csv('data/MultiSeason - from MaxScore/Hermit_Warbler_Gt0.578_2021_max_score_summary.csv')

summary(nchar(encounters.21$Cell_Unit))
# need to add leading zeros for encounters$e2021
for(i in 1:dim(encounters.21)[1]){
  if(nchar(encounters.21$Cell_Unit)[i]==7){
    encounters.21$Cell_Unit[i] = paste("C0", substr(encounters.21$Cell_Unit[i],2,7), sep='')
  } else {
    next
  }
}



## build encounter & effort history ##
all.arus=sort(unique(c( effort.21$Cell_U[effort.21$Cell_U %in% encounters.21$Cell_Unit])))

# compile daily encounter history
encounters=encounters.21[,c(1,80:128)] # hard-coded the dates here
encounters[encounters=="."] <- NA
encounters=as.data.frame(lapply(encounters[,2:50], 
                                as.numeric), row.names = all.arus)

# compile daily effort history


### Model settings and structure ###
{
  min.score="95" # choose "90", "95", or "99"
  min.det.days=3 # if not using an FP model, set a filter for detection days
  
  det.probs=F # if TRUE, reports probabilities, not binary observations. NOTE - currently only works for 90
  round.up.value=0.9995 # if det.probs=T, when is a detection rounded to "1"?
  
  ssp.length=7 # 6 days with a 48-day period was computationally too much
  
  years=1
  
  ## Build empty encounter & effort histories (ssp level)
  sampling.start=seq(from=1, to=n.days.21*years, by=ssp.length)
  sampling.stop=seq(from=ssp.length, to=n.days.21*years, by=ssp.length)
  sample.intervals=length(sampling.start)
  n.ssp=ceiling(n.days.21/ssp.length)
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
          >= thresholds$cutoff95.r_conf[thresholds$species == 'Hermit Warbler']){
        dets[a,s]=1
      } else {
        dets[a,s]=0
      }
    }
    #}
  }
}

head(eff)
#names(habitat)

### Fit models ###
library(unmarked)
evgrUMF = unmarkedFrameOccu(dets, 
                            obsCovs = list(effort = as.data.frame(eff)),
                            siteCovs = habitat )

mod0 <- occu(~1 ~1, evgrUMF)
mod1 <- occu(~effort ~1, evgrUMF)
mod2 <- occu(~1 ~cc_cfo_mn, evgrUMF)
mod3 <- occu(~effort ~cc_cfo_mn, evgrUMF)

modSel(fitList(null=mod0, m1=mod1,m2=mod2,m3=mod3))
summary(mod3)
plot(mod3)


preds = data.frame(cc_cfo_mn=mean(habitat$cc_cfo_mn)) #predict occupancy for mean canopy cover
preds = data.frame(cc_cfo_mn=habitat$cc_cfo_mn) #predict occupancy for canopy cover at each site?
pred_psi = predict(mod3, type="state", newdata=preds, appendData=T)
print(pred_psi)
