library(data.table)
library(fst)
library(magrittr)

rootdir = rprojroot::find_root(rprojroot::has_dir('src'))
datdir = file.path(rootdir, 'data')
resdir = file.path(rootdir, 'results')
output_eu_rivers_dt <- read.fst(file.path(resdir, 'eu_nets', 
                                          'output_eu_rivers_dt.fst')) %>%
  setnames(gsub('date', 'YM', names(.))) 
output_eu_rivers_dt_nodupli <- output_eu_rivers_dt[
  !duplicated(output_eu_rivers_dt$DRYVER_RIV),]


fwrite(output_eu_rivers_dt_nodupli,
       file.path(resdir, 'eu_nets_format', 'output_eu_rivers.csv'))

write_fst(output_eu_rivers_dt_nodupli, file.path(resdir, 'eu_nets_format',
                                                 'output_eu_rivers.fst'))