# chunkify
chunkify <- function(dt, chunksize) {
        
    rows <- nrow(dt)
    chunksize <- as.numeric(chunksize)
    chunks <- 1:ceiling(rows/chunksize)
    chunkid <- unlist(lapply(chunks, function(i) rep(chunks[[i]], chunksize)))
    chunkid <- chunkid[1:rows]
    dt2 <- copy(dt)[, chunkid := chunkid]
    out <- split(dt2, dt2$chunkid)
    return(out)
                                        
}
