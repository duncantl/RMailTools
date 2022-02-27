ff = list.files(pattern = ".*\\.eml", recursive = TRUE, full = TRUE)
mapmj = grep("^./20[0-9]{2}/[3-6]/", ff, value = TRUE)
mapmj = substring(mapmj, 3)
tmp = as.Date(dirname(mapmj), "%Y/%m/%d")


msgs = grep("./GYB-GMail-Backup", ff, value = TRUE, invert = TRUE)
dts = as.Date(gsub("^\\./", "", dirname(msgs)), "%Y/%m/%d")



##########


e = list.files("2020", recursive = TRUE, full = TRUE)
plot(as.integer(gsub("\\./", "", names(tt))), as.integer(tt), ylim = c(0, max(tt)), type = "h")




lapply(list.files("4/24", full = TRUE), readLines, n = 1)




