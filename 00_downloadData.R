if (!dir.exists("data")) dir.create("data")
file_path <- "d0/parasitoid.xlsx"

if(!file.exists(file_path))
download.file("https://dfzljdn9uc3pi.cloudfront.net/2017/3690/1/rawdata.xlsx",
              "data/parasitoid.xlsx")

