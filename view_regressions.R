view_reg_p <- function(x,y,z, single.row = T) {
  require(stargazer)
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "Index.html")
  viewer <- getOption("viewer")
  
  stargazer(x,y,z, out = htmlFile, single.row = single.row, report=('vc*p'))
  
  viewer(htmlFile)
  rm(htmlFile, tempDir, viewer)
}

view_reg <- function(m1 = NA,m2 = NA,m3 = NA,m4 = NA, m5 = NA, covariate.labels = NA, dep.var.labels = NA, single.row = T) {
  require(stargazer)
  if (mode(covariate.labels) == "logical" & mode(dep.var.labels) == "character") {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "Index.html")
    viewer <- getOption("viewer")
    
    stargazer(m1,m2,m3,m4,m5, out = htmlFile, single.row = single.row, 
              dep.var.labels =  dep.var.labels)
    
    viewer(htmlFile)
    rm(htmlFile, tempDir, viewer)
  }
  if (mode(covariate.labels) == "logical" & mode(dep.var.labels) == "logical") {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "Index.html")
    viewer <- getOption("viewer")
    
    stargazer(m1,m2,m3,m4,m5, out = htmlFile, single.row = single.row)
    
    viewer(htmlFile)
    rm(htmlFile, tempDir, viewer)
  }
  if (mode(covariate.labels) == "character" & mode(dep.var.labels) == "logical") {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "Index.html")
    viewer <- getOption("viewer")
    
    stargazer(m1,m2,m3,m4,m5, out = htmlFile, 
              single.row = single.row, covariate.labels = covariate.labels)
    
    viewer(htmlFile)
    rm(htmlFile, tempDir, viewer)
  }
  if (mode(covariate.labels) == "character" & mode(dep.var.labels) == "character") {
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "Index.html")
    viewer <- getOption("viewer")
    
    stargazer(m1,m2,m3,m4,m5, out = htmlFile, single.row = single.row, 
              dep.var.labels =  dep.var.labels,
              covariate.labels = covariate.labels)
    
    viewer(htmlFile)
    rm(htmlFile, tempDir, viewer)
  }
}

view_summary_stats <- function(data, single.row = T) {
  require(stargazer)
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "Index.html")
  viewer <- getOption("viewer")
  
  stargazer(data, summary = TRUE, out = htmlFile, single.row = single.row)
  
  viewer(htmlFile)
  rm(htmlFile, tempDir, viewer)
}
