read.abif = function (filename, max.bytes.in.file =file.info(filename)$size,
   pied.de.pilote = 1.2, verbose = FALSE)
{
 toto=0
   RTC <- function(x, ...) suppressWarnings(rawToChar(x, ...))
   SInt32 <- function(f, ...) readBin(f, what = "integer", signed = TRUE,
       endian = "big", size = 4, ...)
   SInt16 <- function(f, ...) readBin(f, what = "integer", signed = TRUE,
       endian = "big", size = 2, ...)
   SInt8 <- function(f, ...) readBin(f, what = "integer", signed = TRUE,
       endian = "big", size = 1, ...)
   UInt32 <- function(f, ...) readBin(f, what = "integer", signed = FALSE,
       endian = "big", size = 4, ...)
   UInt16 <- function(f, ...) readBin(f, what = "integer", signed = FALSE,
       endian = "big", size = 2, ...)
   UInt8 <- function(f, ...) readBin(f, what = "integer", signed = FALSE,
       endian = "big", size = 1, ...)
   f32 <- function(f, ...) readBin(f, what = "numeric", size = 4,
       ...)
   f64 <- function(f, ...) readBin(f, what = "numeric", size = 8,
       ...)
   fc <- file(filename, open = "rb")
   rawdata <- readBin(fc, what = "raw", n = pied.de.pilote *
       max.bytes.in.file)
   if (verbose)
       print(paste("number of bytes in file", filename, "is",
           length(rawdata)))
   close(fc)
   res <- list(Header = NULL, Directory = NA, Data = NA)
   res$Header$abif <- RTC(rawdata[1:4])
   if (res$Header$abif != "ABIF")
       stop("file not in ABIF format")
   if (verbose)
       print("OK: File is in ABIF format")
   res$Header$version <- SInt16(rawdata[5:6])
   if (verbose)
       print(paste("File in ABIF version", res$Header$version/100))
   res$Header$DirEntry.name <- rawdata[7:10]
   if (verbose)
       print(paste("DirEntry name: ", RTC(res$Header$DirEntry.name)))
   res$Header$DirEntry.number <- SInt32(rawdata[11:14])
   if (verbose)
       print(paste("DirEntry number: ", res$Header$DirEntry.number))
   res$Header$DirEntry.elementtype <- SInt16(rawdata[15:16])
   if (verbose)
       print(paste("DirEntry elementtype: ",res$Header$DirEntry.elementtype))
   res$Header$DirEntry.elementsize <- SInt16(rawdata[17:18])
   if (verbose)
       print(paste("DirEntry elementsize: ",res$Header$DirEntry.elementsize))
   res$Header$numelements <- SInt32(rawdata[19:22])
   if (verbose)
       print(paste("DirEntry numelements: ", res$Header$numelements))
   res$Header$dataoffset <- SInt32(rawdata[27:30])
   if (verbose)
       print(paste("DirEntry dataoffset: ", res$Header$dataoffset))
   dataoffset <- res$Header$dataoffset + 1
   res$Header$datahandle <- SInt32(rawdata[31:34])
   if (verbose)
       print(paste("DirEntry datahandle: ", res$Header$datahandle))
   res$Header$unused <- SInt16(rawdata[35:128], n = 47)
   res$Header$unused[1:length(res$Header$unused)] <- 0
   if (verbose)
       print(paste("DirEntry unused: ", length(res$Header$unused),
           "2-byte integers"))
   dirdf <- data.frame(list(name = character(0)))
   dirdf$name <- as.character(dirdf$name)
   for (i in seq_len(res$Header$numelements)) {
       deb <- (i - 1) * res$Header$DirEntry.elementsize + dataoffset
       direntry <- rawdata[deb:(deb + res$Header$DirEntry.elementsize)]
       dirdf[i, "name"] <- RTC(direntry[1:4])
       dirdf[i, "tagnumber"] <- SInt32(direntry[5:8])
       dirdf[i, "elementtype"] <- SInt16(direntry[9:10])
       dirdf[i, "elementsize"] <- SInt16(direntry[11:12])
       dirdf[i, "numelements"] <- SInt32(direntry[13:16])
       dirdf[i, "datasize"] <- SInt32(direntry[17:20])
       dirdf[i, "dataoffset"] <- SInt32(direntry[21:24])
   }
   if (verbose) {
       print("Element found:")
       print(dirdf$name)
   }
   res$Directory <- dirdf
   res$Data <- vector("list", nrow(dirdf))
   names(res$Data) <- paste(dirdf$name, dirdf$tagnumber, sep = ".")

   print(seq_len(res$Header$numelements))

   for (i in seq_len(res$Header$numelements)) {
       print(i)
       deb <- (i - 1) * res$Header$DirEntry.elementsize + dataoffset
       #print(deb)
       if (dirdf[i, "datasize"] > 4) {
           debinraw <- dirdf[i, "dataoffset"] + 1
       }
       else {
           debinraw <- deb + 20
       }
       elementtype <- dirdf[i, "elementtype"]
       print(elementtype)
       numelements <- dirdf[i, "numelements"]
       print(numelements)
       elementsize <- dirdf[i, "elementsize"]
       data <- rawdata[debinraw:(debinraw + numelements * elementsize)]
       if (elementtype == 1)
           res$Data[[i]] <- UInt8(data, n = numelements)
       if (elementtype == 2){              res$Data[[i]] <- paste(rawToChar(data,multiple=TRUE),collapse="")
         }
       if (elementtype == 3)
           res$Data[[i]] <- UInt16(data, n = numelements)
       if (elementtype == 4)
           res$Data[[i]] <- SInt16(data, n = numelements)
       if (elementtype == 5)
           res$Data[[i]] <- SInt32(data, n = numelements)
       if (elementtype == 7)
           res$Data[[i]] <- f32(data, n = numelements)
       if (elementtype == 8)
           res$Data[[i]] <- f64(data, n = numelements)
       if (elementtype == 10)
           res$Data[[i]] <- list(year = SInt16(data, n = 1),
               month = UInt8(data[-(1:2)], n = 1), day =UInt8(data[-(1:3)],
                 n = 1))
       if (elementtype == 11)
           res$Data[[i]] <- list(hour = UInt8(data, n = 1),
               minute = UInt8(data[-1], n = 1), second =UInt8(data[-(1:2)],
                 n = 1), hsecond = UInt8(data[-(1:3)], n = 1))
       if (elementtype == 18) {
           n <- SInt8(rawdata[debinraw])
           pString <- RTC(rawdata[(debinraw + 1):(debinraw +
               n)])
           res$Data[[i]] <- pString
       }
       if (elementtype == 19)
           res$Data[[i]] <- RTC(data[1:(length(data) - 1)])
       if (elementtype >= 1024)
           res$Data[[i]] <- data
       if (elementtype %in% c(12, 13))
           warning("unimplemented legacy type found in file")
       if (elementtype %in% c(6, 9, 14, 15, 16, 17, 20, 128,
           256, 384))
           warning("unsupported legacy type found in file")
   }
   return(res)
}