q <-"select G.MupolygonWktWgs84 as geom, mapunit.mukey, muname
FROM mapunit
CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) as G
WHERE mukey IN (SELECT DISTINCT mukey FROM component
INNER JOIN coecoclass ON coecoclass.cokey = component.cokey
WHERE ecoclassid = 'R092XY009WI')"

res <- SDA_query(q)

s <- processSDA_WKT(res)

plot(s)

str(res)
unique(res$muname)


library(rgdal)
writeOGR(s, "C:/Paolucci/Projects/ESDs", layer='R092XY009WI', driver='ESRI Shapefile')
