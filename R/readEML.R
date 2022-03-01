# Message Threads.


readEmailMsg.orig =
function(f, x = readLines(f, warn = FALSE), attachments = TRUE)
{
    br = which(x == "")[1]
    # if(is.na(br)) return(x)
#    con = textConnection(x[1:br], local = TRUE, name = 'bob')      #  encoding = "UTF-8
#    con <- tempfile()
#    cat(x[1:br], file = con, sep = "\n")
    hdr = base::read.dcf(textConnection(x[1:br], local = TRUE, name = 'bob'), all = TRUE)
#    hdr = readDCF(x[1:(br-1)])
    m = list(header = hdr, body = x[-(1:br)])
    if(attachments)
        mkAttachments(m, mkAttachment.orig)
    else
        m
}

readEmailMsg =
function(f, x = readLines(f, warn = FALSE), attachments = TRUE, all = TRUE)
{
    br = which(x == "")[1]
    if(is.na(br)) return(x)
    hdr = read.dcf(all = all, lines = x[1:br]) #read.dcf(f, all = all, lines = x[1:br])
    m = list(header = hdr, body = x[-(1:br)])
    if(attachments)
        mkAttachments(m, all = all)
    else
        m
}


readDCF =
function(x)
{
    g = split(x, cumsum(grepl("^[^[:space:]]", x)))
    ans = lapply(g, function(x) { x = paste(x, collapse = " "); structure(trimws(gsub("^[^:]+:(.*)", "\\1",  x)), names = gsub("^([^:]+):.*", "\\1",  x))})
    structure(as.data.frame(ans), names = unname(sapply(ans, names)))
#    matrix(ans, 1, dimnames = list(NULL, unname(sapply(ans, names))))
}

readHeader =
    # not actually used. Would-be caller functions compute the break point for the header and body as they need it for subsequent computations.
function(f, x = readLines(f, warn = FALSE), ...)
{
    br = which(x == "")[1]
    read.dcf(lines = x[1:br], all = TRUE, ...)
}

mkAttachments  =
function(m, make = mkAttachment, all = TRUE)
{
    if(!"Content-Type" %in% names(m$header))
        return(m)

    ty = strsplit(m$header$"Content-Type", ";")[[1]]
    i = grep("boundary", ty)
    if(length(i) == 0)
        return(m)

#XX Handle boundary that has a sequence 0 1 2 3 4 at the end. 
    bndry = trimws(gsub("boundary=", "", ty[i]))
    bndry = gsub('^"|"$', '', bndry)
    bndry = paste0("--", bndry)
    g = grepl(bndry, m$body)

    att = split(m$body, cumsum(g))

    # Make more precise. Has to start with the boundary.    
#    w = grepl(bndry, sapply(att, `[`, 1), fixed = TRUE)
    w = substring(sapply(att, `[`, 1), 1, nchar(bndry)) == bndry
    
    m$body = unlist(att[!w])

    attachments = lapply(att[w], make, bndry)
    attachments = attachments[ ! sapply(attachments, is.null) ]

    multi = sapply(attachments, class) == "list"
    if(any(multi)) {
       attachments = c(attachments[!multi], unlist( attachments[multi], recursive = FALSE))
    }
    # need to unlist if there are elements that are not Attachment, but simple list() of Attachments due to a multipart/alternative
    
    m$att = attachments
    m
}

mkAttachment.orig =
function(lines, boundary, all = TRUE)
{
    if(lines[1] == paste0(boundary, "--") & length(lines[lines != ""]) == 1 )
        return(NULL)

    br = which(lines == "")[1]

    h = list()
    if(br == 2)
        h = base::read.dcf(textConnection(lines[2:(br-1)]), all = all)
    
    list(body = lines[ -(1:br) ], header = h)
}


mkAttachment =
function(lines, boundary, all = TRUE)
{
    if(lines[1] == paste0(boundary, "--") & length(lines[lines != ""]) == 1 )
        return(NULL)

    br = which(lines == "")[1]

    h = read.dcf(lines = lines[2:(br-1)], all = all)
    m = match("Content-type", names(h))
    if(!is.na(m))
        names(h)[m] = "Content-Type"

    if(grepl("boundary", h["Content-Type"]) && grepl("multipart/(alternative|mixed)", h["Content-Type"])) {
        tmp = readEmailMsg(x = lines[-1])
        return(tmp$att)
    }

    structure(list(header = h, body = lines[ -(1:br) ]), class = "Attachment")
}



mkInfoDF =
function(msgs, files = names(msgs))
{
    from = sapply(msgs, function(x) { m = match("from", tolower(names(x$header))); x$header[[m]]})

    efrom = gsub(".* <(.*)>", "\\1", from)

    subject = sapply(msgs, function(x) if("Subject" %in% names(x$header))  x$header$"Subject" else NA)
    to = sapply(msgs, function(x) if("To" %in% names(x$header)) x$header$"To" else NA)

    isR = sapply(msgs, function(x) if("Received" %in% names(x$header)) any(grepl("hypatia.math.ethz.ch", x$header[["Received"]])) else FALSE)

    when = mkDateTime(sapply(msgs, function(x) if("Date" %in% names(x$header)) x$header$"Date" else NA))

    cc = sapply(msgs, function(x) { i = tolower(names(x$header)) == "cc";  if(any(i)) paste(unlist(x$header[i]), collapse = ";") else NA})

    if(length(files) == 0)
        files = rep(as.character(NA), length(from))
    
    mm = data.frame(from = from,
                    to = to,
                    cc = cc,
                    subject = subject,
                    isR = isR,
                    emailFrom = efrom,
                    file = files,
                    numAttachments = sapply(msgs, function(x) length(x$att)),
                    date = when,
                    stringsAsFactors = FALSE)
}

mkDateTime =
function(x)
{
    ans = strptime(x, "%a, %d %b %Y %H:%M:%S %z")
    w = is.na(ans)
    ans[w] = strptime(x[w], "%d %b %Y %H:%M:%S %z")
    ans
}

getContentType =
function(x)
{
    tmp = strsplit(x$header[["Content-Type"]], ";")[[1]]
    tmp = trimws(unique(unlist(tmp)))
    tmp = tmp[ !grepl("boundary|multipart", tmp) ]
   # grep("[^/]pdf", tmp, value = TRUE)
}


saveAttachment =
function(att, to = file.path(dir, getAttachmentName(att)), dir = ".")
{
    val = base64enc::base64decode(att$body)
    Gradhub::savePDF(val, to)
}

getAttachmentName =
function(att, header = att$header)
{
    type = strsplit(header[["Content-Type"]], ";")[[1]]
    m = gregexpr('name="[^"]+"', type)
    ans = regmatches(type, m)
    ans = ans[sapply(ans, length) > 0 ]
    gsub("\n", " ", gsub('name="([^"]+)"', "\\1", ans))
}

readMessages =
function(dir, files = list.files(recursive = TRUE,  full.names  = TRUE), ...)
{
    msgs = lapply(files, readEmailMsg)
}

if(FALSE) {
    # shell:
    #   ag -rl 'S/U|grading option|petition|P/NP' 2020 > SU.2020
    
    source("~/GMail/readEML.R")
    eml = readLines("SU.2020")
    msgs = lapply(eml, readEmailMsg)

    mm = mkInfoDF(msgs)
    w = !mm$isR & grepl("ucdavis.edu", mm$emailFrom, ignore.case = TRUE)
    mm2 = mm[w, ]
    msgs2 = msgs[w]

    tmp = lapply(msgs2, function(x) sapply(x$att, function(x) strsplit(x$header[,"Content-Type"], ";")))
    tmp = trimws((unique(unlist(tmp))))
    tmp = tmp[ !grepl("boundary|multipart", tmp) ]
    grep("[^/]pdf", tmp, value = TRUE)
}


if(FALSE) {
    # Start in the directory
    #    ~/GMail/messages/GYB-GMail-Backup-dtemplelang@ucdavis.edu/
    
    f20 = list.files("2020", recursive = TRUE, full.names = TRUE)
    f20.3.6 = grep("^2020/[3-6]/", f20, value = TRUE)
    system.time({ m20 = lapply(f20.3.6, readEmailMsg, attachments = FALSE) })
    m20df = mkInfoDF(m20, f20.3.6)

    m20df = m20df[!m20df$isR,]
    m20df$fromUCD = grepl("ucdavis.edu", m20df$from)

    m20dfDavis = m20df[m20df$fromUCD,]

    m20Davis = mclapply(as.character(m20dfDavis$file), readEmailMsg, mc.cores = 5)

    m20dfDavis$numAttachments = sapply(m20Davis, function(x) length(x$att))

    atypes = lapply(m20Davis, function(x) lapply(x$att, getContentType))
    hasPDF = sapply(atypes, function(x) any(grepl("/pdf", unlist(x))))

    sort(table(as.character(m20dfDavis$from)[hasPDF]))

    pdfnames = lapply(atypes[hasPDF], function(x) grep("name=", unlist(x), value = TRUE))

    mapply(function(info, pdf) { print(info[, c("from", "to", "file")]); print(pdf); print("*****************************************************")}, split(m20dfDavis[hasPDF,], seq(1, sum(hasPDF))), pdfnames)



    nr = sapply(pdfnames, length)
    tmp = data.frame(pdf = unlist(pdfnames), message = rep(1:length(pdfnames), nr), anum = unlist(lapply(nr, seq_len)), stringsAsFactors = FALSE)

    tmp$pdf = gsub('name="(.*)"$', "\\1", tmp$pdf)



    c("Increased Flexibility for Late Drops & S_U Grad....pdf", "GBSE Cheung Transfer Coursework.pdf",
"tmp2.pdf", "tmp2[1].pdf", "editww.pdf", "MLWG_Apr_28_20.pdf",
"DEB Examiner - Gonzales.pdf", "Gonzales DEB form.pdf", "Deng.EmploymentOffer.S20.pdf",
"gs316-permission-to-drop-petition.pdf", "Summary of Important Considerations for S_U Grading Exceptions.pdf",
"Jake Gonzales - Grad Studies form.pdf", "Diploma_93330434-8f8e-4d54-ab89-82d310a644f2.pdf",
"Diploma_7d77f726-1fa9-4999-ad8c-bf3e758c24a7.pdf", "Satisfactory_Unsatisfactory Grading for Spring 2020.pdf",
"petition.pdf", "filled2.pdf", "GGHA_Spring2020_Policy.docx",
"GGHA_Advisor S_U guidance Spring 2020.pdf", "gs316-permission-to-drop-petition_Ochoa_hij.pdf",
"2621_001.pdf", "Jaleel 2020 CV[1].pdf", "Transcript (9) (9).pdf",
"gs337-variable-unit-change-petition-AAR.pdf", "Cristiana signed Form S U.pdf",
"Reed,M grade option change spring 2020[3].pdf", "S20 PHI 237 S U GRADE FORM FOR - TAMRAS, RAMIEL (999575549).pdf",
"Reed,M grade option change spring 2020[3] KGEE.pdf")
  pdfs2get =  c() 

    # all dealt with. The 3 below came in June 4th.
NEW = c(    
"Cristiana signed Form S U.pdf",
"S20 PHI 237 S U GRADE FORM FOR - TAMRAS, RAMIEL (999575549).pdf",
"Reed,M grade option change spring 2020[3] KGEE.pdf")

    m = match(pdfs2get, tmp$pdf)
    mid = tmp$message[m]

    mapply(function(m, i) length(m$att) >= i,  m20Davis[hasPDF][mid], tmp$anum[mid])
    
    mapply(function(pdf, msg, name, anum) {
        anum = which(sapply(msg$att, getAttachmentName) == pdf)
        if(length(anum) == 0)
            stop("problem")
        saveAttachment(msg$att[[anum]], name)
    },  pdfs2get, m20Davis[hasPDF][mid], file.path("SUPDF3", tmp$pdf[m]), tmp$anum[m])
}
