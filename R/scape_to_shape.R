ScapetoShape <- function(x,t=0.42,d=1000,f=1000,s=3){

    c1 <- raster::raster(paste0(x,".tif"))
    c1c <- raster::cut(c1,breaks=c(-Inf,t,Inf))
    c1p <- raster::rasterToPolygons(c1c,function(j){j==2},dissolve=TRUE)
    sp::proj4string(c1p) <- sp::CRS("+proj=longlat +datum=WGS84")
    c1p <- sf::st_as_sf(c1p)

    c1pd <- smoothr::drop_crumbs(c1p,threshold=units::set_units(d, km^2))
    c1pe <- smoothr::fill_holes(c1pd,threshold=units::set_units(f, km^2))
    c1smooth <- smoothr::smooth(c1pe,method="ksmooth",smoothness=s)
    c1smooth <- sf::as_Spatial(c1smooth,'Spatial')
    rgdal::writeOGR(c1smooth, dsn=".",
                    layer = paste0(x),
                    driver = "ESRI Shapefile")
  }
