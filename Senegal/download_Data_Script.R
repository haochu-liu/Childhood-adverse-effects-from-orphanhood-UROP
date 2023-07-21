library(rdhs)

set_rdhs_config(email = "yujia.luan21@imperial.ac.uk",
                project = "Childhood adverse effects from orphanhood")

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

SN15HM<-get_datasets(
  dataset_filenames="SNPR7HFL.zip"
)
SN15HM<-readRDS(SN15HMDL$"SNPR7HFL")

SN86HW<-get_datasets(
  dataset_filenames="SNHW02FL.zip"
)
SN86HW<-readRDS(SN86HW$"SNHW02FL")

SN92HM<-get_datasets(
  dataset_filenames="SNPR21FL.zip"
)
SN92HM<-readRDS(SN92HM$"SNPR21FL")

SN97HM<-get_datasets(
  dataset_filenames="SNPR32FL.zip"
)
SN97HM<-readRDS(SN97HM$"SNPR32FL")

SN99HH<-get_datasets(
  dataset_filenames="SNHH41FL.zip"
)
SN99HH<-readRDS(SN99HH$"SNHH41FL")

SN05HH<-get_datasets(
  dataset_filenames="SNPR4AFL.zip"
)
SN05HM<-readRDS(SN05HH$"SNPR4AFL")

SN06HH<-get_datasets(
  dataset_filenames="SNPR51FL.zip"
)
SN06HM<-readRDS(SN06HH$"SNPR51FL")

SN08HH<-get_datasets(
  dataset_filenames="SNPR5AFL.zip"
)
SN08HM<-readRDS(SN08HH$"SNPR5AFL")

SN10HH<-get_datasets(
  dataset_filenames="SNPR61FL.zip"
)
SN10HM<-readRDS(SN10HH$"SNPR61FL")

SN12HH<-get_datasets(
  dataset_filenames="SNPR6DFL.zip"
)
SN12HM<-readRDS(SN12HH$"SNPR6DFL")

SN14HH<-get_datasets(
  dataset_filenames="SNPR70FL.zip"
)
SN14HM<-readRDS(SN14HH$"SNPR70FL")

SN15HH<-get_datasets(
  dataset_filenames="SNPR7HFL.zip"
)
SN15HM<-readRDS(SN15HH$"SNPR7HFL")

SN16HH<-get_datasets(
  dataset_filenames="SNPR7IFL.zip"
)
SN16HM<-readRDS(SN16HH$"SNPR7IFL")

SN17HH<-get_datasets(
  dataset_filenames="SNPR7ZFL.zip"
)
SN17HM<-readRDS(SN17HH$"SNPR7ZFL")

SN18HH<-get_datasets(
  dataset_filenames="SNPR81FL.zip"
)
SN18HM<-readRDS(SN18HH$"SNPR81FL")

SN19HM<-get_datasets(
  dataset_filenames="SNPR8BFL.zip"
)
SN19HM<-readRDS(SN19HM$"SNPR8BFL")

SN20HM<-get_datasets(
  dataset_filenames="SNPR8IFL.zip"
)
SN20HM<-readRDS(SN20HM$"SNPR8IFL")

