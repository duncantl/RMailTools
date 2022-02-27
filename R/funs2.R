fileDate =
    # date from file name
function(f)
{
    e = strsplit(f, "/")
    tmp = sapply(e, function(x) { n = length(x) - 1; paste(x[seq(n -2, n)], collapse = "/")})
    as.Date(tmp, "%Y/%m/%d")
}
