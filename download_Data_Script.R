library(rdhs)
#configuration
set_rdhs_config(email = "yujia.luan21@imperial.ac.uk" ,
                project = "Childhood adverse effects from orphanhood")
#get available datasetes
get_available_datasets(clear_cache=FALSE)

#download 20-21household datasets
SN2021HHDL<-get_datasets(
  dataset_filenames="SNHR8IFL.zip"
)
#download 20-21children datasets
SN2021CHDL<-get_datasets(
  dataset_filenames="SNKR8IFL.zip"
)
SN2021HMDL<-get_datasets(
  dataset_filenames="SNPR8IFL.zip"
)
SN2021IDL<-get_datasets(
  dataset_filenames="SNIR8IFL.zip"
)
SN2021CH<-readRDS(SN2021CHDL$"SNKR8IFL")
SN2021HH<-readRDS(SN2021HHDL$"SNHR8IFL")
SN2021HM<-readRDS(SN2021HMDL$"SNPR8IFL")
SN2021I<-readRDS(SN2021IDL$"SNIR8IFL")

#2019DATA
#download 20-21household datasets
SN19HHDL<-get_datasets(
  dataset_filenames="SNHR8BFL.zip"
)
#download 20-21children datasets
SN19CHDL<-get_datasets(
  dataset_filenames="SNKR8BFL.zip"
)
SN19HMDL<-get_datasets(
  dataset_filenames="SNPR8BFL.zip"
)
SN19IDL<-get_datasets(
  dataset_filenames="SNIR8BFL.zip"
)
SN19CH<-readRDS(SN19CHDL$"SNKR8BFL")
SN19HH<-readRDS(SN19HHDL$"SNHR8BFL")
SN19HM<-readRDS(SN19HMDL$"SNPR8BFL")
SN19I<-readRDS(SN19IDL$"SNIR8BFL")

SN15HMDL<-get_datasets(
  dataset_filenames="SNPR7HFL.zip"
)
SN15HM<-readRDS(SN15HMDL$"SNPR7HFL")
