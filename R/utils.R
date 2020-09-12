
make_dir=function(datapath){
  if (!dir.exists(datapath)) {
    cat("Creating path", paste0("./",datapath,"/\n"))
    dir.create(datapath, recursive = T)
  }
}
