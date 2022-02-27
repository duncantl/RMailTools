getAttachedFiles =
function(msg, ...)
{
    ans = lapply(msg$att, getAttachmentFile, ...)
    ans[!sapply(ans, is.null)]
}

getAttachmentFile =
function(att, types = c("application", "image"))    
{
    m = match("content-type", tolower(names(att$header)))
    ty = att$header[1, m]
    els = strsplit(ty, "; *")[[1]]
    #    app = gsub("application/", "", grep("application", els, value = TRUE))
    ty = strsplit(els[[1]], "/")[[1]]

    if(ty[1] %in% types   &&   ty[2] %in% c("pdf", "doc", "docx", "png")) 
        structure(att$body, filename = getFileName(att$header))
}

getFileName =
function(h)    
{
    p = c("Content-Type" = "name", "Content-Disposition" = "filename")
    for(i in names(p)) {
      if(i %in% names(h)) {
          info = getInfo(h[1, i])
          if(p[i] %in% names(info))
              return(gsub('^"|"$', '',  info[ p[i] ]))
      }
    }
    
    "" # NA
}

getInfo =
function(d)
{
    els = strsplit(d, ";[[:space:]]*")[[1]]
    els = strsplit(els, "=")
    els = structure(sapply(els, function(x) x[length(x)]), names = sapply(els, `[`, 1))
}
