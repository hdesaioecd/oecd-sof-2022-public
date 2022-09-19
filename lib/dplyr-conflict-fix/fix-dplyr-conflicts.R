fix.dplyr.conflicts = function(folder = "."){
    filenames = list.files(folder, full.names = T)[file_ext(list.files(folder, full.names = T))=="R"]
    for( f in filenames ){
        
        x <- readLines(f)
        x <- gsub( "%>% filter", "%>% dplyr::filter", x)
        x <- gsub( "%>% select", "%>% dplyr::select", x )
        x <- gsub( "%>% rename", "%>% dplyr::rename", x )
        x <- gsub( "%>% summarise", "%>% dplyr::summarise", x )
        cat(x, file=f, sep="\n")
        
    }
    
}
