dir = "~/GMail/messages/GYB-GMail-Backup-dtemplelang@ucdavis.edu/2022/2"
ff = list.files(dir, recursive = TRUE, full = TRUE, pattern = "\\.eml$")
msgs = lapply(ff, function(x) try(readEmailMsg(x)))
names(msgs) = ff
info = mkInfoDF(msgs)
info$file = ff
# i = grep("excellence", info$subject, ignore.case = TRUE)


#################################################

msgs.att = msgs[ info$numAttachments > 0 ]
attachments = unlist(lapply(msgs.att, `[[`, "att"), recursive = FALSE)

ct = unlist(lapply(msgs.att, function(x) sapply(x$att, function(x) x$header["Content-Type"])))
tmp = data.frame(type = tolower(sapply(strsplit(ct, ";[[:space:]]*"), `[`, 1)), 
                 msgNum = rep(1:length(msgs.att), sapply(msgs.att, function(x) length(x$att))))

info.att = mkInfoDF(msgs.att)
info.att$msgNum = 1:nrow(info.att)

i = grep("(/pdf|wordprocessing)", tmp[,1])

tmp2 = info.att[tmp[i, 2],  ]
tmp3 = tmp2[ !grepl("tgiardino|dtemplelang|rvmartino|hewest|lhmcdiarmid|chegrant|ad-assistant", tmp2$from), ]

j = grep("award|excellence|recommendation",  tmp3$subject, ignore.case = TRUE)
tmp3[j, c("subject", "from")]
pd.info = tmp3[j, c("subject", "from")]


ww = tmp$msgNum %in% tmp3$msgNum[j] & grepl("(/pdf|wordprocessing)", tmp[,1])

rownames(tmp[ww, ])


pd.attachments = attachments[ as.integer(rownames(tmp[ww, ])) ]

dir = "~/OGS/Awards/PostdocAward/2022/LOR"
file.remove(list.files(dir, full = TRUE))
fn = paste0( gsub("/", "_", gsub("[[:space:]]+", "_", pd.info$subject)), ".pdf")
w3 = fn == "Re:_2022_UC_Davis_Award_for_Excellence_in_Postdoctoral_Research_Nomination.pdf"


fn = sapply(pd.attachments, function(x) getFileName(x$header))
fn = gsub("/", "_", gsub("[[:space:]]+", "_", fn))

out = file.path("~/OGS/Awards/PostdocAward/2022/LOR", fn)

o = mapply(function(x, to) Gradhub::savePDF(base64decode(x$body), to), pd.attachments, out)

#############################

if(FALSE) {
w = grepl("excellence in postdoctoral|recommendation|excellence|post doc award", info$subject, ignore.case = TRUE) & !grepl("tgiardino|dtemplelang", info$from)

pd.info = info[w,]
files = lapply(msgs[w], getAttachedFiles, "application")
w2 = sapply(files, length) > 0

filenames = sapply(files[w2], function(x) attr(x[[1]], "filename"))
filenames[filenames==""] = paste0( gsub(" ", "_", pd.info$subject[w2][filenames == ""]), ".pdf")

o = mapply(function(x, to) Gradhub::savePDF(base64decode(x[[1]]), to), files[w2], file.path("~/OGS/Awards/PostdocAward/2022/LOR", filenames))
}
