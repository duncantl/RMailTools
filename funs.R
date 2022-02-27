getFrom =
function(f, ll = readLines(f, warn = FALSE), h = getHeader(f, ll))
{
    h[1, "From"]
}

getHeader =
function(f, ll = readLines(f, warn = FALSE))
{
    i = which(ll == "")[1]
    h = read.dcf(textConnection(ll[1:(i-1)]), all = TRUE)
}
    
