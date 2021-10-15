setwd('D:/NYU/Classes/1006_capstone/project/code')

source('Monet.R')
library(data.table)

## Read in data
ecol = read.csv('../data/combined_data.csv')

## Run Monet
mn = Monet$new(raw_data = ecol, experiment_name = 'Text Experiment', verbose = TRUE)

mn$save_schema('../data/initial_schema.xlsx')

write.csv(mn$data, "../data/monet_output.csv")
saveRDS(mn, "../output/monet_output.rds")
saveRDS(mn$schema, "../output/schema.rds")



mn <- readRDS("../output/schema.rds")






## INOUT - change to boolean (but change variable name to match)
## SURGSPEC - skipped
## CNSCOMA - make sure ' ' values are also mapped to 0 (though we probably drop this anyways)
  ## NEURODEF
  ## OTHGRAFL

## COL_ORAL_ANTIBIOTIC_UNK is technically binary
  ## COL_*_UNK features, probably

## Probably drop PROPER30

## What about CDARREST? Binary or not?

## DROP:
## ATTEND
## PACKS
## ETOH
## DNR
## FNSTATUS1
## CPNEUMON
## ESOVAR
## HXMI
## PRVPCI
## PRVPCS
## HXANGINA
## HXPVD
## RESTPAIN
## IMPSENS
## COMA
## HEMI
## HXTIA
## CVA
## CVANO
## TUMORCNS
## QUAD


## PGY?

## Eren phone 7189741816
## Make sure to include and check Anesthesia
## COL_APPROACH: binary: open -or- minimally invasive surgery






