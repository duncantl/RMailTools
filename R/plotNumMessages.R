function(dirs = 2009:2022, ...)
{    
    ff = lapply(as.character(dirs), function(y) list.files(y,  recursive = TRUE, full.names = TRUE))
    ff2 = unlist(ff)
    g = split(ff2, dirname(ff2))
    num = sapply(g, length)
    z = data.frame( d = as.Date(names(num)), num = num)
    z2 = z[order(z$d), ]

    plot(z2$d, z2$num, type = "l", ylim = c(0, 400), ...)
    invisible(z2)
}

