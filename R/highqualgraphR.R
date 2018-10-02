#' Publication-grade export of ggplot2 object
#'
#' This function takes a ggplot2 object and a series of extensions as input
#' to export it to. A high quality graph is generated complying to PLOS ONE
#' author guidelines.
#'
#' @param x a ggplot2 object.
#' @param filename the filename without an extension.
#' @param res resolution (ppi) of the graph, defaults to 1200
#' @param pointsize font size (pixels), defaults to 12
#' @param extension character vector including: "pdf", "png", "tiff", "svg"
#'  or "postscript"
#' @param embed a logical indicating wether in the font should be embedded
#'  in a pdf or eps document. This is done to enhance compatibility with older
#'  viewers or when using a non-standard font (see ?embedFonts for details). It
#'  requires ghostscript to be installed.
#' @param graphicstype passed on to the "type" parameter in bitmap graphics
#' devices (defaults to "cairo", but requires cairo graphics)
#' @param colorspace sets the colorspace. For printing such as in papers
#'  the default colorspace should be CMYK but for on-screen viewing srgb may
#'  be more desirable. Can also be set to gray to enable greyscale printing.
#'  Defaults to CMYK.
#' @importFrom assertthat is.string
#' @importFrom ggplot2 is.ggplot
#' @importFrom grDevices pdf png tiff svg postscript dev.off
#' @importFrom extrafont embed_fonts
#' @examples
#' ## Short example
#'
#' # Load precomputed example data
#' #TODO: find test object
#'
#' @export




highqualgraphR<-function(x,filename,res=1200,pointsize=12,embed=FALSE,
                         extension = "pdf", graphicstype="cairo",
                         colorspace="cmyk")
{
  #sanity check for filename
  assertthat::assert_that(assertthat::is.string(filename))
  #see ?assertthat:assert_that and ?assertthat:is.string
  #### Check wether x is a ggplot object ####
  if(! ggplot2::is.ggplot(x)){stop("your object (x) is not a ggplot object")}
  #### Check whether or not a supported extension is given ####
  suportedextensions<-c("pdf", "png", "tiff", "svg","postscript")
  extensioncheck <- extension %in% suportedextensions
  if(length(extension)==1)
  {
    stopifnot(sum(extensioncheck)==1)
  }
  else
  {
    stopifnot(sum(extensioncheck)==length(extension))
  }
  #### check whether or not extrafonts is loaded if not check if installed ####
  #this is rendered moot by using an R package
  # if(!("package:extrafont" %in% search())){
  #   if (!(require("extrafont", character.only=TRUE, quietly=TRUE))) {
  #     print("The extrafont package is not installed or could not be loaded")
  #   }else{
  #     extrafont::font_import()
  #     extrafont::loadfonts(quiet=TRUE)
  #     extrafont::loadfonts(device = "postscript",quiet=TRUE)
  #   }
  # }
  extrafont::loadfonts(quiet=TRUE)
  extrafont::loadfonts(device = "postscript",quiet=TRUE)
  #######################################################################
  # If you receive Error:
  #     unknown family 'Arial'
  # Then run the following code:
  #
  # library("extrafont")
  # font_import()
  # loadfonts()
  # loadfonts(device = "postscript")
  # don't forget to embed fonts for backwards compatibility with all pdf
  # and postscript viewers
  # If you receive Error:
  # Error in embedFonts(...) GhostScript was not found
  # on windows run: Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.06/bin/gswin64.exe")
  # This requires to have ghostscript installed on your windows machine
  #######################################################################

  # Create subfolder figure to save your files
  working_directory <- getwd()
  subdirectory <- "figure/"
  dir.create(file.path(working_directory, subdirectory), showWarnings = FALSE)

  filename <- paste(subdirectory, filename, sep = "")

  # define function to print your graphs
  print_graph <- function(format){
    if (format == "pdf")
    {
      pdf(file=paste(filename,".pdf",sep=""), height = 6, width = 9,
          family = "Arial", paper = "special", onefile = FALSE,
          colormodel=colorspace)
    }
    else if (format == "png")
    {
      png(filename=paste(filename,".png",sep=""),width=12000,height=7000,
          units="px",res=res,pointsize=pointsize,family="Arial",
          type=graphicstype)
    }
    else if (format == "tiff")
    {
      tiff(filename=paste(filename,".tiff",sep=""),width=12000,height=7000,
           res=res,pointsize=pointsize,family="Arial",type=graphicstype,
           compression="lzw")
    }
    else if (format == "svg")
    {
      svg(filename=paste(filename,".svg",sep=""),width=9,height=6,
          pointsize=12,family="Arial")
    }
    else if (format == "postscript")
    {
      postscript(file=paste(filename,".eps",sep=""), height = 6, width = 9,
                 family = "Arial", paper = "special", onefile = FALSE,
                 horizontal = FALSE,colormodel=colorspace)
    }
    else
    {
      stop(paste(filename," is not a valid extension"))
    }
    # Print your files
    print(x)
    dev.off()
    if(embed==TRUE){
      if (format == "pdf"){
        embed_fonts(file = paste(filename,".pdf",sep="") ,
                    outfile = paste(filename,"_embed.pdf",sep=""))
      }else if(format=="postscript")
      {
        embed_fonts(file = paste(filename,".eps",sep=""),
                    outfile = paste(filename,"_embed.eps",sep=""))
      }
    }#default is FALSE. Ghostscript is generally not installed on windows systems.
  }

  # Check whether to print one or multiple plots
  if (is.vector(extension))
  {
    for (format in extension)
    {
      print_graph(format)
    }
  }
  else
  {
    print_graph(extension)
  }
}
