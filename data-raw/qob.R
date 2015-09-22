#Fetch data from Angrist's data repository and save as a dataset for testing and examples.

#Download and extract the data from Angrist's repository
download.file("http://economics.mit.edu/files/2853", "data-raw/qob.rar")
system("unrar x data-raw/qob.rar data-raw/")

qob = read.csv("data-raw/QOB", sep=" ", stringsAsFactors=FALSE)
names(qob)[1] = "AGE"
names(qob)[2] = "AGESQ"
names(qob)[4] = "EDUC"
names(qob)[5] = "ENOCENT"
names(qob)[6] = "ESOCENT"
names(qob)[9] = "LWKLYWGE"
names(qob)[10] = "MARRIED"
names(qob)[11] = "MIDATL"
names(qob)[12] = "MT"
names(qob)[13] = "NEWENG"
names(qob)[16] = "CENSUS"
names(qob)[18] = "QOB"
names(qob)[19] = "RACE"
names(qob)[20] = "SMSA"
names(qob)[21] = "SOATL"
names(qob)[24] = "WNOCENT"
names(qob)[25] = "WSOCENT"
names(qob)[27] = "YOB"

#keep.vars = c("LWKLYWGE", "AGE", "AGESQ", "EDUC", "QOB", "YOB")

#Other variables not kept
keep.vars = c("LWKLYWGE", "AGE", "AGESQ", "EDUC", "QOB", "YOB",
              "ENOCENT", "ESOCENT", "MARRIED", "MIDATL", "MT", "NEWENG",
              "CENSUS", "RACE", "SMSA", "SOATL", "WNOCENT", "WSOCENT")
#

#Keep only 1930s YOB
qob = dplyr::filter(qob, YOB>29, YOB<40)[,keep.vars]

#Save the data as an R dataset
devtools::use_data(qob, compress="bzip2", overwrite=TRUE)

#Remove the extracted data
system("rm data-raw/QOB")
