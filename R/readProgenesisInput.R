#' reads file exportet from progenesis
#' ProgenesisRead
#' @export
#' @param file path to progenesis peptide or protein file
#' @param sep separator used (progenesis uses depending language settings a , or ;)
#' @import plyr
#' @import readr
#' @import stringr
#' @examples
#' 
#' file = file.path(path.package("quantable"),"extdata/PG/PeptideMeasurement_inclSingleHits_hi3.csv" )
#' tmp <- ProgenesisRead(file)
#' colnames(tmp)
#' head(tmp)
#'
#' file = file.path(path.package("quantable"),"extdata/PG/ProteinMeasurement_inclSingleHits_hi3.csv" )
#' tmp <- ProgenesisRead(file)
#' colnames(tmp)
#' head(tmp)
#' 
ProgenesisRead <- function(file, sep=","){
  tmp<-readLines(file)
  types <- stringr::str_trim(stringr::str_split(gsub("\"","",tmp[1]),pattern=sep)[[1]])
  annot <- stringr::str_trim(stringr::str_split(gsub("\"","",tmp[2]),pattern=sep)[[1]])
  
  fixTypesAnnot <- function(types){
    wtype<-which(stringr::str_length(types)> 0)
    ltype<-diff(c(wtype,length(types)+1))
    x<-rep(types[wtype],times=ltype)
    
    types[ wtype[1]:(wtype[1]+length(x)-1) ] <- x
    return(types)
  }
  
  types <- fixTypesAnnot(types)
  annot <- fixTypesAnnot(annot)
  types[annot == "Tags"] <-""
  
  annot <- paste(types, annot,sep="_")
  data <- readr::read_delim(file,delim=sep, skip=2)
  
  colnames(data)<- paste(annot,colnames(data),sep="~")
  colnames(data) <- gsub("^_~","",colnames(data))
  
  Acc <- strsplit(data$Accession,split=";")
  Acc <- sapply(Acc, function(x){x[1]})
  
  data$ProteinName <- data$Accession
  data$TopProteinName <- Acc
  
  data <- plyr::rename(data,c("Unique peptides"="nrPeptides"))
  #data <- data[,-grep("Normalized abundance",colnames(data))]
  return(data)
}
#' build annotation from column names
#' @param data tibble returned by ProgenesisRead
#' @return list of tibbles data - 
#' @export
#' @examples
#' 
#' file = file.path(path.package("quantable"),"extdata/PG/ProteinMeasurement_inclSingleHits_hi3.csv" )
#' tmp <- ProgenesisRead(file)
#' colnames(tmp)
#' xx <- ProgenesisBuildAnnotation(tmp)
#' head(xx$anno)
#' colnames(xx$data)
#' colnames(tmp)
ProgenesisBuildAnnotation <- function(data){
  ### Build annotation
  annV <- colnames(data)
  annV<-grep("Raw abundance",colnames(data),value=T)
  annV<-gsub("Raw abundance_","",annV)
  annV<-gsub("_1$","",annV)
  ann<-data.frame(Condition=quantable::split2table(annV,split="~")[,1],
                  Raw.file=quantable::split2table(annV,split="~")[,2],
                  stringsAsFactors = FALSE)
  ann$Run<-1:nrow(ann)
  ann$IsotopeLabelType <- "Light"
  ann$BioReplicate  <- 1:nrow(ann)
  
  ## Fix names
  newnames <-gsub("Raw abundance_.+~", "Intensity.", colnames(data))
  newnames <- gsub("_1$","", newnames)
  
  colnames(data) <- newnames
  return(list(data = data, annotation = ann))
}
