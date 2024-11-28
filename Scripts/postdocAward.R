invisible(lapply(file.path("~/GMail/RMailTools/R", c("readEML.R", "read.dcf.R", "attachments.R")), source))
source("~/GMail/RMailTools/Scripts/postdocFuns.R")

dir = "~/GMail/messages/GYB-GMail-Backup-dtemplelang@ucdavis.edu/2022/2"
ff = list.files(dir, recursive = TRUE, full = TRUE, pattern = "\\.eml$")
w = basename(dirname(ff)) %in% 17:28
ff = ff[w]

dir2 = "~/GMail/messages/GYB-GMail-Backup-dtemplelang@ucdavis.edu/2022/3"
ff2 = list.files(dir, recursive = TRUE, full = TRUE, pattern = "\\.eml$")
ff = c(ff, ff2)

msgs0 = msgs = lapply(ff, function(x) try(readEmailMsg(x)))
#names(msgs) = ff
info = mkInfoDF(msgs)
info$file = ff
# i = grep("excellence", info$subject, ignore.case = TRUE)

to.us = (grepl("dtemplelang|tgiardino", info$to ) | grepl("dtemplelang|tgiardino", info$cc )) & !grepl("dtemplelang|tgiardino", info$from)

info = info[to.us,]
msgs = msgs[to.us]


#################################################

msgs.att = msgs[ info$numAttachments > 0 ]
attachments = unlist(lapply(msgs.att, `[[`, "att"), recursive = FALSE)

# Each row in tmp corresponds to an attachment.
ct = unlist(lapply(msgs.att, function(x) sapply(x$att, function(x) x$header["Content-Type"])))
tmp = data.frame(type = tolower(sapply(strsplit(ct, ";[[:space:]]*"), `[`, 1)), 
                 msgNum = rep(1:length(msgs.att), sapply(msgs.att, function(x) length(x$att))))


# info.att - each row corresponds to an email, not an attachment.
info.att = info[ info$numAttachments > 0, ]   #mkInfoDF(msgs.att)
info.att$msgNum = 1:nrow(info.att)
info.att$award = sapply(msgs.att, function(x) try(isAwardMsg(x))) | grepl("serres@ucr.edu|crhale@ucsb.edu", info.att$from)

i = grep("(/pdf|wordprocessing|msword)", tmp[,1])

tmp2 = info.att[tmp[i, 2],  ]
tmp3 = tmp2[ !grepl("tgiardino|dtemplelang|rvmartino|hewest|lhmcdiarmid|chegrant|ad-assistant|delplanque", tmp2$from), ]

j = grepl("award|excellence|recommendation",  tmp3$subject, ignore.case = TRUE) | tmp3$award


#tmp3[j, c("subject", "from")]
pd.info = tmp3[j, c("subject", "from")]


ww = tmp$msgNum %in% tmp3$msgNum[j] & grepl("(/pdf|wordprocessing|msword)", tmp[,1])

rownames(tmp[ww, ])

#XXXX
pd.attachments = attachments[ as.integer(rownames(tmp[ww, ])) ]

dir = "~/OGS/Awards/PostdocAward/2022/LOR"
if(FALSE) {
file.remove(list.files(dir, full = TRUE))
fn = paste0( gsub("/", "_", gsub("[[:space:]]+", "_", pd.info$subject)), ".pdf")
w3 = fn == "Re:_2022_UC_Davis_Award_for_Excellence_in_Postdoctoral_Research_Nomination.pdf"
}

fn = sapply(pd.attachments, function(x) getFileName(x$header))
fn = gsub("/", "_", gsub("[[:space:]]+", "_", fn))

out = file.path("~/OGS/Awards/PostdocAward/2022/LOR", fn)

#library(base64enc)
o = mapply(function(x, to) Gradhub::savePDF(base64enc::base64decode(x$body), to), pd.attachments, out)

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
