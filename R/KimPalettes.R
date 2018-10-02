#' Request discrete color palettes created by Kim De Paepe
#'
#' Function to select a discrete color palette as created by Kim De Paepe using
#' to create color palettes: http://tools.medialab.sciences-po.fr/iwanthue/ .
#' The options are 2, 3, 4,5, 6,7,8,9,10,11,12,15,16,20,30 and 100-color palettes.
#' @param n number of colors in the plot (needs to be in the pre-defined list
#' in the description)
#' @return a character vector with hexadecimal color definitions of the desired
#'  length
#' @examples
#' ## Short example
#'
#' fourcolours <- KimPalettes(4)
#' plot(runif(4),cex=3,pch=16,col=fourcolours)
#' @export

KimPalettes <- function(n){
  if(!n %in% c(2,3,4,5,6,7,8,9,10,11,12,15,16,20,30,100)){
    stop(paste(n, "is not a valid number of colors for pallettes currently implemented in KimPalettes.\n Please visit http://tools.medialab.sciences-po.fr/iwanthue/ to make your own palette.\n"))
  }
  twocolors     <- c("#9BD0E3","#DFDA5F")
  threecolors   <- c("#323C4D","#A03D44","#98B736")
  fourcolors    <- c("#4E4340","#85C85B","#A463B5","#C04F4C")
  fivecolors    <- c("#e6444f","#3863d9","#f4d24e","#880034","#702300")
  sixcolors     <- c("#4E4340","#85C85B","#A463B5","#C04F4C","#94B8B7",
                     "#BB9749")
  sevencolors   <- c("#9BD0E3","#DFDA5F","#9FDF9D","#E3B1D2","#E3B573",
                     "#6EDFC4","#B2E078")
  eightcolors   <- c("#9BD0E3","#DFDA5F","#9FDF9D","#E3B1D2","#E3B573",
                     "#6EDFC4","#B2E078","#008B00")
  eightcolors   <- c("#7B4B71","#C5CD42","#C44F44","#91B7C0","#484934",
                     "#BA9C5F","#B167C8","#69C467")
  ninecolors    <- c("#C8ADA4","#7FCD50","#AB5CC7","#BB5236","#50354B",
                     "#535F37","#CCB24D","#C54F80","#7E91C5")
  tencolors     <- c("#C8ADA4","#7FCD50","#AB5CC7","#BB5236","#50354B",
                     "#535F37","#CCB24D","#C54F80","#7E91C5","#7CCCAC")
  elevencolors  <- c("#000000","#CD6600","#36648B","#008B00","#7A378B",
                     "#838B83","#8B0000","#323C4D","#A03D44","#98B736",
                     "#C8ADA4")
  twelvecolors  <- c("#000000","#CD6600","#36648B","#008B00","#7A378B",
                     "#838B83","#8B0000","#323C4D","#A03D44","#98B736",
                     "#C8ADA4","#BB5236")
  fifteencolors <- c("#000000","#CD6600","#36648B","#008B00","#7A378B",
                     "#838B83","#8B0000","#323C4D","#C9A325","#A03D44",
                     "#98B736","#C8ADA4","#BB5236","#7FCD50")
  twelvecolors  <- c("#008B00","#7A378B","#838B83","#8B0000","#323C4D",
                     "#A03D44","#98B736","#C8ADA4","#BB5236","#7FCD50",
                     "#AB5CC7","#C9A325")
  sixteencolors <- c("#000000","#CD6600","#36648B","#008B00","#7A378B",
                     "#838B83","#8B0000","#1FD5C4","#A03D44","#98B736",
                     "#C8ADA4","#BB5236","#7FCD50","#AB5CC7","#C9A325",
                     "#323C4D")
  thirtycolors  <- c("#323F24","#CB51D7","#72E245","#DE4F2D","#81DDC7",
                     "#6584C6","#CFAE3C","#914261","#BFDC86","#CDC6BE",
                     "#D3469A","#3C315A","#607B30","#DD4469","#5B9072",
                     "#CA9FC7","#7F592D","#5A656D","#D1867F","#4C2426",
                     "#79B6CE","#6E67D1","#CBDC3F","#63D98B","#97362B",
                     "#CBB37D","#7E3F85","#CF8338","#5DA73A","#CB80D0")
  twentycolors  <- c("#D3469A","#3C315A","#607B30","#DD4469","#5B9072",
                     "#CA9FC7","#7F592D","#5A656D","#D1867F","#4C2426",
                     "#79B6CE","#6E67D1","#CBDC3F","#63D98B","#97362B",
                     "#CBB37D","#7E3F85","#CF8338","#5DA73A","#CB80D0")
  hundredcolors <- c("#7eb96b","#9366e9","#6eb729","#b55edb","#46c353",
                     "#a92fa4","#afc52e","#3468e5","#b4a823","#5a4fc4",
                     "#82cf62","#e357bc","#4b9925","#cf6bd6","#369039",
                     "#e53486","#39c685","#bc2c84","#8eab32","#6b4bb1",
                     "#e1b338","#6574de","#ea952a","#4691eb","#e96f2d",
                     "#366bb3","#bfbf51","#a475e0","#64862b","#8f449d",
                     "#839331","#ed84da","#428b50","#e32851","#47cebe",
                     "#bc2d1e","#59bfec","#ec5438","#3db6c0","#e93e73",
                     "#6ebf92","#ef5fa0","#349176","#d73e50","#3793c2",
                     "#c14d17","#6c99d9","#c37524","#988ce5","#b79c36",
                     "#d58fe9","#4d6b26","#9f6bbb","#887e22","#714e98",
                     "#b58427","#5f5ba3","#e2a35a","#42548c","#ee915a",
                     "#727ab3","#a7571b","#aaa4e5","#585a16","#e09fd9",
                     "#2a6a45","#d44375","#a1b86c","#bf5b98","#bbb16c",
                     "#96487e","#888f50","#b7365d","#69672d","#b785c4",
                     "#7b5d16","#f18eb6","#a07f3d","#c980ae","#dab27a",
                     "#9b3956","#8f7042","#cf6589","#8f5527","#8e5170",
                     "#f3775a","#85424d","#ee9f81","#aa3a3a","#c18c63",
                     "#d5687b","#c77c48","#e48c8f","#97411f","#f48671",
                     "#a46442","#dd696c","#af615c","#d45945","#c76952")
  if(n==2){
    return(twocolors)
  }else if(n==3){
    return(threecolors)
  }else if(n==4){
    return(fourcolors)
  }else if(n==5){
    return(fivecolors)
  }else if(n==6){
    return(sixcolors)
  }else if(n==7){
    return(sevencolors)
  }else if(n==8){
    return(eightcolors)
  }else if(n==9){
    return(ninecolors)
  }else if(n==10){
    return(tencolors)
  }else if(n==11){
    return(elevencolors)
  }else if(n==12){
    return(twelvecolors)
  }else if(n==15){
    return(fifteencolors)
  }else if(n==16){
    return(sixteencolors)
  }else if(n==20){
    return(twentycolors)
  }else if(n==30){
    return(thirtycolors)
  }else if(n==100){
    return(hundredcolors)
  }
}
