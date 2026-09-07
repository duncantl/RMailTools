# Look through my emails (within a given date period) and find the
# emails *I sent* with the subject "FERPA training required for Graduate Faculty Advisor position"
# Then we can determine who we have already notified.
# See Rcode/checkFERPA.R

getEmailsByDate =
function(start = as.Date("2026/8/27"), end = Sys.Date(), dir = "~/GMail/messages/GYB-GMail-Backup-dtemplelang@ucdavis.edu")
{
    days = seq(start, end)
    days2 = format(days, "%Y/%m/%d")
    days2 = gsub("/0", "/", days2)

    unlist(lapply(days2,
                  function(d)
                      list.files(file.path(dir, d), pattern = "\\.eml$", recursive = FALSE, full = TRUE)))
}

headerDf =
function(hdrs)
{
    d = getFromHeaders(hdrs, "Date")
    dt = mkDate(d)
    data.frame(
        to = getFromHeaders(hdrs, "To"),
        from = getFromHeaders(hdrs, "From"),
        subject = getFromHeaders(hdrs, "Subject"),
        date = dt,
        cc = getFromHeaders(hdrs, "Cc")        
        )
}

mkDate =
function(d)
{
    dt = as.POSIXct(strptime(d, "%a, %d %b %Y %H:%M:%S"))
    w = is.na(dt)
    dt[w] = as.POSIXct(strptime(d[w], "%d %b %Y %H:%M:%S"))
    dt
}

getFromHeaders =
function(hdrs, fieldName)    
{
    ans = sapply(hdrs, function(x) orNA(x[1, fieldName]))
    w = is.na(ans)
    if(any(w)) {
        fieldName = tolower(fieldName)
        ans[w] = sapply(hdrs[w], function(x) orNA(x[1, fieldName]))
    }
    # Cc and CC
    w = is.na(ans)
    if(any(w)) {
        fieldName = toupper(fieldName)
        ans[w] = sapply(hdrs[w], function(x) orNA(x[1, fieldName]))
    }    

    ans
}
