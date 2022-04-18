library(rgee)

ee_Initialize()

maskL8sr <- function(image) {
  # Bit 0 - Fill
  # Bit 1 - Dilated Cloud
  # Bit 2 - Cirrus
  # Bit 3 - Cloud
  # Bit 4 - Cloud Shadow
  qaMask = image$select('QA_PIXEL')$bitwiseAnd(strtoi('11111', 2))$eq(0)
  saturationMask = image$select('QA_RADSAT')$eq(0)

  # Apply the scaling factors to the appropriate bands
  opticalBands = image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
  thermalBands = image$select('ST_B.*')$multiply(0.00341802)$add(149.0)

  # Replace the original bands with the scaled ones and apply the masks.
  out = image$addBands(opticalBands, NULL, TRUE)
  out = out$addBands(thermalBands, NULL, TRUE)
  out = out$updateMask(qaMask)
  out = out$updateMask(saturationMask)

  return(out)
}

landsat <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
landsat <- landsat$filterDate(start = "2021-06-01", opt_end = "2021-09-01")
best <- landsat$map(maskL8sr)
best <- best$median()

vizParams <- list(
  bands = c('SR_B7', 'SR_B5', 'SR_B3'),
  gamma = 1.5
)

Map$setCenter(lon = -115, lat = 56, zoom = 10)
Map$addLayer(best, vizParams, 'false color composite')
