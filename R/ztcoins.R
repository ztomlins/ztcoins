## Load the sample data set
coins = read.csv("OCREcoins.csv")

#' Looks for emeperor in database
#'
#' Searches a coin database for the total number of coins featuring emperor
#' @param empname takes a string in quotes
#'
#' @return the total number of coins featuring a particular emperor
#'
#' @examples
#' empnumber("Maximian")
#'
#' @export
empnumber <- function(empname)
{
  filteremp <- grep(empname, coins$Portrait) # Filter database by name provided
  numemp <- length(filteremp) # Get vector length for total number in database
  return(numemp) # Return total
}

#' Looks for a deity in database
#'
#' Searches a coin database for the total number of coins featuring deity
#' @param deityname takes a string in quotes
#'
#' @return the total number of coins featuring a particular deity
#'
#' @examples
#' deitynumber("Venus")
#'
#' @export
deitynumber <- function(deityname)
{
  filterdeity <- grep(deityname, coins$Deity)
  numdeity <- length(filterdeity)
  return(numdeity)
}

#' Looks for a denomination type in database
#'
#' Searches a coin database for the total number of coins of a particular denomination
#' @param denom takes a string in quotes
#'
#' @return the total number of coins of chosen denomination
#'
#' @examples
#' denomnumber("Denarius")
#'
#' @export
denomnumber <- function(denom)
{
  filterdenom <- grep(denom, coins$Denomination)
  numdenom <- length(filterdenom)
  return(numdenom)
}

#' Looks for a material type in database
#'
#' Searches a coin database for the total number of coins made from a material
#' @param mat takes a string in quotes
#'
#' @return the total number of coins featuring made from a particular material
#'
#' @examples
#' matnumber("Gold")
#'
#' @export
matnumber <- function(mat)
{
  filtermat <- grep(mat, coins$Material)
  nummat <- length(filtermat)
  return(nummat)
}

#' Looks for a mint location in database
#'
#' Searches a coin database for the total number of coins minted at a city
#' @param mint takes a string in quotes
#'
#' @return the total number of coins minted at a chosen city
#'
#' @examples
#' mintnumber("Trier")
#'
#' @export
mintnumber <- function(mint)
{
  filtermint <- grep(mint, coins$Mint)
  nummint <- length(filtermint)
  return(nummint)
}

#' Looks for a region in database
#'
#' Searches a coin database for the total number of coins minted in a region
#' @param region takes a string in quotes
#'
#' @return the total number of coins from a chosen region
#'
#' @examples
#' regionnumber("Gallia")
#'
#' @export
regionnumber <- function(region)
{
  filterregion <- grep(region, coins$Region)
  numregion <- length(filterregion)
  return(numregion)
}

#' Looks for a year in database
#'
#' Searches a coin database for the total number of coins minted in a year
#' @param year takes a string in quotes
#'
#' @return the total number of coins minted in a chosen year
#'
#' @examples
#' yearnumber("145")
#'
#' @export
yearnumber <- function(year)
{
  filteryear <- grep(year, coins$Year)
  numyear <- length(filteryear)
  return(numyear)
}

#' Plot the number of coins minted in each mint
#'
#' Computes the number of coins minted at each mint and plots it as a bar plot
#' @param none
#'
#' @return A bar plot of the results
#'
#' @examples
#' mints()
#'
#' @export
mints <- function()
{
  mintsbar <- coins$Mint[grepl("",coins$Mint)]
  barplot(table(mintsbar),main="Mints of the Roman Empire",xlab="Mint",ylab="Count",las=2)
}

#' Plot the coins minted in each material
#'
#' Computes the number of coins minted in each material and plots it as a bar plot
#' @param none
#'
#' @return A bar plot of the results
#'
#' @examples
#' materials()
#'
#' @export
materials <- function()
{
  matsbar <- coins$Material[grepl("",coins$Material)]
  barplot(table(matsbar),main="Metals of Coins",xlab="Mint",ylab="Count")
}

#' Plot the emperors appearing on coins
#'
#' Computes the number of times an emperor appears on coins in the database as a bar plot
#' @param none
#'
#' @return A bar plot of the results
#'
#' @examples
#' emperors()
#'
#' @export
emperors <- function()
{
  empsbar <- coins$Portrait[grepl("",coins$Portrait)]
  barplot(table(empsbar),main="Imperators of the Roman Empire",xlab="Emperor",ylab="Count",las=2)
}

#' Plot the denominations of coins
#'
#' Computes the number of each type of denomination in a database as a bar plot
#' @param none
#'
#' @return A bar plot of the results
#'
#' @examples
#' denominations()
#'
#' @export
denominations <- function()
{
  denombar <- coins$Denomination[grepl("",coins$Denomination)]
  barplot(table(denombar),main="Denominations of Roman Coins",xlab="Denomination",ylab="Count",las=2)
}

