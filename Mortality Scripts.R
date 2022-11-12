# Credits to the demography package which I sourced the hmd.mx function from because it wont run directly from the library
hmd.mx <- function(country, username, password, label=country)
{
    path <- paste("https://former.mortality.org/hmd/", country, "/STATS/", "Mx_1x1.txt", sep = "")
    userpwd <- paste(username, ":", password, sep = "")
    txt <- RCurl::getURL(path, userpwd = userpwd)
    con <- textConnection(txt)
    mx <- try(utils::read.table(con, skip = 2, header = TRUE, na.strings = "."),TRUE)
    close(con)
    if(class(mx)=="try-error")
        stop("Connection error at www.mortality.org. Please check username, password and country label.")

    path <- paste("https://former.mortality.org/hmd/", country, "/STATS/", "Exposures_1x1.txt", sep = "")
    userpwd <- paste(username, ":", password, sep = "")
    txt <- RCurl::getURL(path, userpwd = userpwd)
    con <- textConnection(txt)
    pop <- try(utils::read.table(con, skip = 2, header = TRUE, na.strings = "."),TRUE)
    close(con)
    if(class(pop)=="try-error")
        stop("Exposures file not found at www.mortality.org")

    obj <- list(type="mortality",label=label,lambda=0)

    obj$year <- sort(unique(mx[, 1]))
    #obj$year <- ts(obj$year, start=min(obj$year))
    n <- length(obj$year)
    m <- length(unique(mx[, 2]))
    obj$age <- mx[1:m, 2]
    mnames <- names(mx)[-c(1, 2)]
    n.mort <- length(mnames)
    obj$rate <- obj$pop <- list()
    for (i in 1:n.mort)
    {
        obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
        obj$rate[[i]][obj$rate[[i]] < 0] <- NA
        obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
        obj$pop[[i]][obj$pop[[i]] < 0] <- NA
        dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
    }
    names(obj$pop) = names(obj$rate) <- tolower(mnames)

    suppressWarnings(obj$age <- as.numeric(as.character(obj$age)))
    if (is.na(obj$age[m]))
        obj$age[m] <- 2 * obj$age[m - 1] - obj$age[m - 2]
    return(structure(obj, class = "demogdata"))
}

#' @rdname hmd
#' @export