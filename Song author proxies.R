getSongAuthorProxies <- function(i.lennon, i.mccartney, i.harrison, i.starkey) {
  # Songs authored by one of the Beatles
  # Use set operations:                       # <x>, <y>: vectors of the same mode
  # <intersection> <- intersect(<x>, <y>)
  # <set difference> <- setdiff(<x>, <y>)
  mccartney.songs <- setdiff(i.mccartney, i.lennon)
  mccartney.songs <- setdiff(mccartney.songs, intersect(mccartney.songs, i.harrison))
  mccartney.songs <- setdiff(mccartney.songs, intersect(mccartney.songs, i.starkey))
  # length(mccartney.songs)
  # the.beatles.songs$Title[mccartney.songs]
  # the.beatles.songs$Songwriter[mccartney.songs]
  
  lennon.songs <- setdiff(i.lennon, i.mccartney)
  lennon.songs <- setdiff(lennon.songs, intersect(lennon.songs, i.harrison))
  lennon.songs <- setdiff(lennon.songs, intersect(lennon.songs, i.starkey))
  # length(lennon.songs)
  # the.beatles.songs$Title[lennon.songs]
  # the.beatles.songs$Songwriter[lennon.songs]
  
  harrison.songs <- setdiff(i.harrison, i.mccartney)
  harrison.songs <- setdiff(harrison.songs, intersect(harrison.songs, i.lennon))
  harrison.songs <- setdiff(harrison.songs, intersect(harrison.songs, i.starkey))
  # length(harrison.songs)
  # the.beatles.songs$Title[harrison.songs]
  # the.beatles.songs$Songwriter[harrison.songs]
  
  starkey.songs <- setdiff(i.starkey, i.mccartney)
  starkey.songs <- setdiff(starkey.songs, intersect(starkey.songs, i.harrison))
  starkey.songs <- setdiff(starkey.songs, intersect(starkey.songs, i.lennon))
  # length(starkey.songs)
  # the.beatles.songs$Title[starkey.songs]
  # the.beatles.songs$Songwriter[starkey.songs]
  
  # Songs authored by two or more of the Beatles
  # Lennon/McCartney and McCartney/Lennon songs:
  lennon.mccartney.songs <- intersect(i.lennon, i.mccartney)
  lennon.mccartney.songs <- setdiff(lennon.mccartney.songs, i.harrison)
  lennon.mccartney.songs <- setdiff(lennon.mccartney.songs, i.starkey)
  # the.beatles.songs$Title[lennon.mccartney.songs]
  # the.beatles.songs$Songwriter[lennon.mccartney.songs]
  mccartney.lennon.songs <- lennon.mccartney.songs
  lennon.mccartney.songs <- 
    lennon.mccartney.songs[startsWith(the.beatles.songs$Songwriter[lennon.mccartney.songs], "Lennon")]
  mccartney.lennon.songs <- 
    mccartney.lennon.songs[startsWith(the.beatles.songs$Songwriter[mccartney.lennon.songs], "McCartney")]
  # Lennon/McCartney/Harrison/Starkey songs
  lennon.mccartney.harrison.starkey.songs <- 
    intersect(intersect(intersect(i.lennon, i.mccartney), i.harrison), i.starkey)
  
  authors <- list(lennon.songs, mccartney.songs, harrison.songs, starkey.songs, 
                  lennon.mccartney.songs, mccartney.lennon.songs, lennon.mccartney.harrison.starkey.songs)
  authors
}
