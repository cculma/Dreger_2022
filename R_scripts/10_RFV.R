
# DDM = digestible dry matter
# DDM = digestible dry matter
# DMI = dry matter intake
# DMI = dry matter intake
# RFV = Relative Feed Value
# RFQ = Relative Forage Quality
# NDF = neutral detergent fiber (% of DM)
# NDFD = 48-hour in vitro NDF digestibility (% of NDF)
# RFV = (DDM * DMI) / 1.29

# DDM = (88.9 - (.779 * % ADF))
# DMI = (120/% aNDF)

# RFV = (DMI) * (DDM) /1.29

# DMI = 120/ NDF + (NDFD - 45) * 374 / 1350 * 100

120/a2$aNDF + (a2$dNDF48 - 45) * 374 / 1350 * 100
hist(a2$aNDF)
hist(a2$ADF)

DMI = (120/a2$aNDF)
a2$RFV <- ((88.9 - (.779 * a2$ADF)) * (120/a2$aNDF)) / 1.29
