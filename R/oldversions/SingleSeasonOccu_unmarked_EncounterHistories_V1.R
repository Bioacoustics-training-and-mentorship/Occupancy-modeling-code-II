#### Fit some occupancy models! 
# Load required library
library(unmarked)

# Read in .csv files
effort_days <- read.csv('data/SingleSeason - from pre-made inputs/effort_days.csv')
head(effort_days)

effort_hrs <- read.csv('data/SingleSeason - from pre-made inputs/effort_hrs.csv')
head(effort_hrs)

eh_NUWO <- read.csv('data/SingleSeason - from pre-made inputs/Nubian Woodpecker.csv')
head(eh_NUWO)

eh_ASCI <- read.csv('data/SingleSeason - from pre-made inputs/Ashy Cisticola.csv')
head(eh_ASCI)

eh_GRWH <- read.csv('data/SingleSeason - from pre-made inputs/Green Woodhoopoe.csv')
head(eh_GRWH)

eh_AFPI <- read.csv('data/SingleSeason - from pre-made inputs/African Pipit.csv')
head(eh_AFPI)

## Nubian Woodpecker
occ_NUWO <- unmarkedFrameOccu(eh_NUWO, siteCovs = NULL, 
                              obsCovs = list(hours=effort_hrs, 
                                             days=effort_days)) 

head(occ_NUWO)

# Fit models
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