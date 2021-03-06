 MODULE Constants

!============================!
!  GENERAL PARAMETER VALUES  !
!============================!
   Integer, parameter :: resample = 2000 ! Number of resampling
! Reservoir size(0 represents the reference value)
   double precision, parameter ::   P0 = 3.1d15      ! Marine PO4 [molP]
   double precision, parameter ::   N0 = 43.5d15     ! Marine NO3 [molN]
   double precision, parameter ::   O0 = 37d18       ! O2 in the Ocean-Atmosphere [mol]
   double precision, parameter ::   A0 = 3.193d18    ! CO2&DIC in the Ocean-Atmosphere [mol]
   double precision, parameter ::   G0 = 1.25d21     ! Crustal org C [molC]
   double precision, parameter ::   C0 = 5d21        ! Crustal carbonate C [molC]
   double precision, parameter ::   S0 = 40d18       ! Marine SO4 [molS]
   double precision, parameter :: PYR0 = 0.18d21     ! Crustal pyrite S [molS]
   double precision, parameter :: GYP0 = 0.2d21      ! Crustal gypsum S [molS]
   double precision, parameter :: CAL0 = 13.97d18    ! Marine Ca2+ [mol]
   double precision, parameter :: ALK0 = 3.2d18      ! Alkalinity in the ocean [mol]
   double precision, parameter :: Mocean = 1.476d21  ! Mass of the ocean []
! Atmospheric composition
   double precision, parameter :: pCO20 = 280d0      ! CO2 [ppmv]
   double precision, parameter :: pO20  = 0.21d0     ! O2 [atm]
   double precision, parameter :: mO20  = 0.21d0     ! O2 [mixing ratio]
   double precision, parameter :: pN2O  = 265d0      ! N2O []
   double precision, parameter :: pCH40 = 0.715d-6   ! CH4 [atm]
   double precision, parameter :: pCH400= 0.715d0    ! CH4 [ppmv]
! Fluxes (W = weathering, B = burial, Fm = metamorphic degassing)
   double precision, parameter :: Wcarb0  = 13.35d12 ! Carbonate weathering [mol/yr]
   double precision, parameter :: Wsil0   =  6.65d12 ! Silicate weathering [mol/yr]
   double precision, parameter :: Bcarb0  = Wsil0 + Wcarb0 ! Alkalinity balance
   double precision, parameter :: Bcorg0  = 4.5d12   ! Burial rate of organic C in the ocean [molC/yr]
   double precision, parameter :: BtCorg0 = 4.5d12   ! Burial rate of terrigenous organic C [molC/yr]
   double precision, parameter :: Fmorg0  = 1.25d12  ! CO2 outgassing via metamorphism of organic C [molC/yr]
   double precision, parameter :: Wgyp0   = 1d12     ! Gypsum weathering [molS/yr]
   double precision, parameter :: Wpyr0   = 0.53d12  ! Pyrite weathering [molS/yr]
   double precision, parameter :: Fnfix0  = 8.72d12  ! Nitrogen fixation
   double precision, parameter :: Fdeni0  = 8.6d12   ! Denitrification
   double precision, parameter :: Bpfe0   = 0.006d12 ! Burial rate of Fe-bound P
   double precision, parameter :: Bpca0   = 0.015d12 ! Burial rate of auth-P
   double precision, parameter :: Jch4t0  = 1d12     ! CH4 flux from terrestrial biosphere
   double precision, parameter :: Po0     = 225.96d0 ! New production []
   double precision, parameter :: NPP0    = 3750d12  ! Marine NPP [molC/yr]
   double precision, parameter :: NPPt0   = 5000d12  ! Terrestrial NPP [molC/yr]
! Stoichiometry
   double precision, parameter :: CPland0 = 1000d0   ! Corg/Porg ratio of land plants
   double precision, parameter :: CNorg0  = 37.5d0   ! Corg/Norg ratio of buried sediments
   double precision, parameter :: CPorg0  = 250d0    ! Corg/Porg ratio of buried sediments
   double precision, parameter :: CPoxic  = 217d0    ! Corg/Porg ratio of buried oxic sediments
! Other
   double precision, parameter :: phi = 0.01614d0    ! 
   double precision, parameter :: DelCmax = 33d0     ! Max value of C isotopic fractionation
   double precision, parameter :: DelC0 = 24d0       ! 
   double precision, parameter :: J = 5d0            ! 
   double precision, parameter :: DelP0 = 19d0       ! 
   double precision, parameter :: DelS0 = 35d0       ! 
   double precision, parameter :: sig = 5.67d-8      ! 
   double precision, parameter :: Sun0 = 1368d0      ! Solar constant [W/m2]
   double precision, parameter :: tau = 4.55d9       ! 
   double precision, parameter :: k16 = 3.762d0      ! 
   double precision, parameter :: kfire = 3d0        ! Lenton (2013)
   double precision, parameter :: DOA0 = 0.14d0      ! Ref value of degree of anoxia
   double precision, parameter :: Tc0 = 15d0         ! 
   double precision, parameter :: sesc = 3.7d-5      ! 
   double precision, parameter :: PoL0 = 4700d12     ! 
   double precision, parameter :: pi = dacos(-1d0)    ! Circular constant
   double precision, parameter :: etaCH4 = 0.0288d0  ! 
   double precision, parameter :: Topt = 28d0        ! 
   double precision, parameter :: dTopt = 28d0       ! 
   double precision, parameter :: rCP = 117d0        ! C/P stoichiometry
   double precision, parameter :: rNP =  16d0        ! N/P stoichiometry
   double precision, parameter :: const2 = 1.274796748d0 
   double precision, parameter :: Kv = 366d0         ! 
   double precision, parameter :: Kp2 = 153.597d0    ! 
   double precision, parameter :: dr = 13.6d18       ! 
   double precision, parameter :: ds =  5.0d18       ! 
   double precision, parameter :: dss=  1.0d18       ! 
   double precision, parameter :: dd = 0.273d18      ! 
   double precision, parameter :: Tice = 263d0       ! 
   double precision, parameter :: T0 = 273d0         ! 
   double precision, parameter :: aice = 0.65d0      ! 
   double precision, parameter :: a00 = 0.35d0       ! 
   double precision, parameter :: mu = 0.6d0         ! 
   double precision, parameter :: a_ch4 = 1d0/11.07d0 ! 
   double precision, parameter :: b_ch4 = 2.4d0      ! 
   double precision, parameter :: c3 = 0.95d0        ! 
   double precision, parameter :: c4 = 1d0 - c3      ! 
   double precision, parameter :: RUNcarb = 0.087d0  !
   double precision, parameter :: k11 = BtCorg0 / (BtCorg0+(Bcorg0/CPorg0+Bpfe0+Bpca0)*CPland0) ! Fraction of P entering to the terrestrial biosphere
   double precision, parameter :: Wp0 = (Bcorg0/CPorg0+Bpfe0+Bpca0) / (1d0-k11) ! P weathering [molP/yr]
   double precision, parameter :: Pland0 = Wp0 * k11   ! P flux to terrestrial biosphere [molP/yr]
   double precision, parameter :: Tk0 = Tc0 + 273.15d0 ! Surface temp. [K]

   double precision, parameter :: gmolN2 = 28d0    ! g/mol
   double precision, parameter :: gmolO2 = 32d0    ! g/mol
   double precision, parameter :: gmolAr = 40d0    ! g/mol
   double precision, parameter :: gmolCO2 = 44d0   ! g/mol
   double precision, parameter :: gmolCH4 = 16d0   ! g/mol
   double precision, parameter :: fN20 = 0.78d0    ! 
   double precision, parameter :: fO20 = 0.21d0    !  
   double precision, parameter :: fAr0 = 0.01d0    ! 
   double precision, parameter :: fCO20 = 280d-6   !
   double precision, parameter :: fCH40 = 0.715d-6 ! 
   double precision, parameter :: Matm_g0 = 5.2d21 ! g
   double precision, parameter :: gmolatm0 = fN20*gmolN2 + fO20*gmolO2 + fAr0*gmolAr + fCO20*gmolCO2 + fCH40*gmolCH4
   double precision, parameter :: Matm_mol0 = Matm_g0 / gmolatm0 ! mol
   double precision, parameter :: Matm_N20 = fN20*Matm_mol0      ! mol
   double precision, parameter :: Matm_O20 = fO20*Matm_mol0      ! mol
   double precision, parameter :: Matm_Ar0 = fAr0*Matm_mol0      ! mol
   double precision, parameter :: Matm_CO20 = fCO20*Matm_mol0    ! mol
   double precision, parameter :: Matm_CH40 = fCH40*Matm_mol0    ! mol
   double precision, parameter :: Rearth = 6.4d6  ! m
   double precision, parameter :: gravity = 9.8d0 ! m/s^2
   double precision, parameter :: const = gravity / (4d0*pi*Rearth**2d0) * 1d-3 / 1.013d5

!========================!
!  FITTING COEFFICIENTS  !
!========================!
   double precision, parameter :: a1ch4 = 0.0030084d0
   double precision, parameter :: a2ch4 =-0.1655405d0
   double precision, parameter :: a3ch4 = 3.2305351d0
   double precision, parameter :: a4ch4 =-25.8343054d0
   double precision, parameter :: a5ch4 = 71.5397861d0

   double precision, parameter :: a1  = 9.12805643734088612007d0
   double precision, parameter :: a2  = 4.58408794776474781685d0
   double precision, parameter :: a3  =-84.7261075511868995136d0
   double precision, parameter :: a4  = 0.435517381110739065786d0
   double precision, parameter :: a5  =-28.6355036266497364750d0
   double precision, parameter :: a6  = 296.626642450453971378d0
   double precision, parameter :: a7  =-0.0601082900358798077889d0
   double precision, parameter :: a8  =-2.60414691486032312540d0
   double precision, parameter :: a9  = 56.9812976578495309354d0
   double precision, parameter :: a10 =-462.596100050751886101d0
   double precision, parameter :: a11 = 2.18159373001554097310d-3
   double precision, parameter :: a12 = 0.161456772400849241089d0
   double precision, parameter :: a13 = 3.75623788186383888998d0
   double precision, parameter :: a14 =-35.3347289235212116409d0
   double precision, parameter :: a15 = 275.011005363597746509d0
!
   double precision, parameter :: b1  =-0.441391619954555503025d0
   double precision, parameter :: b2  =-0.260017516002879089942d0
   double precision, parameter :: b3  = 1.08110772295329837789d0
   double precision, parameter :: b4  =-3.93863285843020910493d-2
   double precision, parameter :: b5  =-1.46383456258096611435d0
   double precision, parameter :: b6  = 9.91383778608142668398d-2
   double precision, parameter :: b7  =-1.45914724229303338632d0
   double precision, parameter :: b8  =-2.72769392852398387395d-2
   double precision, parameter :: b9  =  3.99933641081463919775d0
   double precision, parameter :: b10 = 1.07231336256525633388d0
   double precision, parameter :: b11 =-1.04302520934751417891d-2
   double precision, parameter :: b12 =-6.10296439299006454604d0
   double precision, parameter :: b13 = 2.69255203910960137434d-3
   double precision, parameter :: b14 = 9.50143253373007257157d-2
   double precision, parameter :: b15 = 7.37864215757422226005d0
   double precision, parameter :: b16 = 0.128580729156335171748d0
   double precision, parameter :: b17 =-0.307800300913486257759d0
   double precision, parameter :: b18 = 2.27715594632176554502d-2
   double precision, parameter :: b19 = 0.611699085276039222769d0
   double precision, parameter :: b20 =-2.33213409642421742873d0
   double precision, parameter :: b21 = 0.256011431303802661219d0
   double precision, parameter :: b22 = 10.5912148222549546972d0
   double precision, parameter :: b23 =-1.85772688884413561539d-2
   double precision, parameter :: b24 =-0.755796861024326749323d0
   double precision, parameter :: b25 =-11.6485004141808623501d0
   double precision, parameter :: b26 = 27.4062491988752192640d0
   double precision, parameter :: b27 = 0.546044240911252587445d0
   double precision, parameter :: b28 =-205.761674358916081928d0
   double precision, parameter :: b29 = 5.57943359123403426203d-2
   double precision, parameter :: b30 =-2.49880329758542751861d0
   double precision, parameter :: b31 = 514.448995054491206247d0
   double precision, parameter :: b32 = 2.43702089287719950508d-3
   double precision, parameter :: b33 =-0.109384840764980617589d0
   double precision, parameter :: b34 = 2.92643187434628071486d0
   double precision, parameter :: b35 =-427.802454850920923946d0

!  Williams and Kasting (1997) :: TOA (190 K < T < 280 K)
   double precision, parameter :: z1  =-0.68910d0
   double precision, parameter :: z2  = 1.0460d0
   double precision, parameter :: z3  = 7.8054d-3
   double precision, parameter :: z4  =-2.8373d-3
   double precision, parameter :: z5  =-0.28899d0
   double precision, parameter :: z6  =-3.7412d-2
   double precision, parameter :: z7  =-6.3499d-3
   double precision, parameter :: z8  = 0.20122d0
   double precision, parameter :: z9  =-1.8508d-3
   double precision, parameter :: z10 = 1.3649d-4
   double precision, parameter :: z11 = 9.8581d-5
   double precision, parameter :: z12 = 7.3239d-2
   double precision, parameter :: z13 =-1.6555d-5
   double precision, parameter :: z14 = 6.5817d-4
   double precision, parameter :: z15 = 8.1218d-2
!  Williams and Kasting (1997) :: TOA (280 K < T < 370 K)
   double precision, parameter :: zz1  = 1.1082d0
   double precision, parameter :: zz2  = 1.5172d0
   double precision, parameter :: zz3  =-5.7993d-3
   double precision, parameter :: zz4  = 1.9705d-2
   double precision, parameter :: zz5  =-0.1867d0
   double precision, parameter :: zz6  =-3.1355d-2
   double precision, parameter :: zz7  =-1.0214d-2
   double precision, parameter :: zz8  = 0.20986d0
   double precision, parameter :: zz9  =-3.7098d-3
   double precision, parameter :: zz10 =-1.1335d-4
   double precision, parameter :: zz11 = 5.3714d-5
   double precision, parameter :: zz12 = 7.5887d-2
   double precision, parameter :: zz13 = 9.269d-6
   double precision, parameter :: zz14 =-4.1327d-4
   double precision, parameter :: zz15 = 6.3298d-2
!  Williams and Kasting (1997) :: jOLR (W/m2) (190 K < T < 380 K)
   double precision, parameter :: x1  = 9.46898d0
   double precision, parameter :: x2  =-7.714727d-5
   double precision, parameter :: x3  =-2.794778d0
   double precision, parameter :: x4  =-3.244753d-3
   double precision, parameter :: x5  =-3.547406d-4
   double precision, parameter :: x6  = 2.212108d-2
   double precision, parameter :: x7  = 2.229142d-3
   double precision, parameter :: x8  = 3.088497d-5
   double precision, parameter :: x9  =-2.789815d-5
   double precision, parameter :: x10 =-3.442973d-3
   double precision, parameter :: x11 =-3.361939d-5
   double precision, parameter :: x12 = 9.173169d-3
   double precision, parameter :: x13 =-7.775195d-5
   double precision, parameter :: x14 =-1.679112d-7
   double precision, parameter :: x15 = 6.590999d-8
   double precision, parameter :: x16 = 1.528125d-7
   double precision, parameter :: x17 =-3.367567d-2
   double precision, parameter :: x18 =-1.631909d-4
   double precision, parameter :: x19 = 3.663871d-6
   double precision, parameter :: x20 =-9.255646d-9

   double precision, parameter :: p00 = 1.712d0
   double precision, parameter :: p10 =-0.3212d0
   double precision, parameter :: p01 =-1.97d0
   double precision, parameter :: p20 =-0.2595d0
   double precision, parameter :: p11 = 0.02261d0
   double precision, parameter :: p02 = 0.6206d0
   double precision, parameter :: p30 = -0.01508d0
   double precision, parameter :: p21 = 0.1081d0
   double precision, parameter :: p12 =-0.03527d0
   double precision, parameter :: p03 =-0.1487d0
   double precision, parameter :: p40 = 0.003142d0
   double precision, parameter :: p31 =-0.003905d0
   double precision, parameter :: p22 =-0.01894d0
   double precision, parameter :: p13 = 0.01487d0
   double precision, parameter :: p04 = 0.01797d0
   double precision, parameter :: p50 = 0.0001997d0
   double precision, parameter :: p41 =-0.000598d0
   double precision, parameter :: p32 = 0.0001878d0
   double precision, parameter :: p23 = 0.001942d0
   double precision, parameter :: p14 =-0.001568d0
   double precision, parameter :: p05 =-0.0009482d0
!  pCH4 < 1e-6 bar
   double precision, parameter :: c0l6 =-57265.34429022d0
   double precision, parameter :: c1l6 =-34227.66598214d0
   double precision, parameter :: c2l6 =-8511.63546063d0 
   double precision, parameter :: c3l6 =-1126.69934250d0
   double precision, parameter :: c4l6 =-83.71121714d0
   double precision, parameter :: c5l6 =-3.30919540d0
   double precision, parameter :: c6l6 =-0.05436644d0
   double precision, parameter :: c0m6 =-6.00815080d0
   double precision, parameter :: c1m6 = 18.02266008d0 
   double precision, parameter :: c2m6 = 9.13915748d0
   double precision, parameter :: c3m6 = 2.19756947d0
   double precision, parameter :: c4m6 = 0.28479353d0
   double precision, parameter :: c5m6 = 0.01904814d0
   double precision, parameter :: c6m6 = 0.00051560d0
   double precision, parameter :: c0h6 =-21.50360309d0
   double precision, parameter :: c1h6 =-0.84092722d0
   double precision, parameter :: c2h6 = 0.00101886d0
   double precision, parameter :: c3h6 =-0.02287496d0
!  pCH4 = 1e-5 bar
   double precision, parameter :: c0l5 =-40362.30026998d0
   double precision, parameter :: c1l5 =-23417.68514816d0
   double precision, parameter :: c2l5 =-5652.97793198d0
   double precision, parameter :: c3l5 =-726.55027862d0
   double precision, parameter :: c4l5 =-52.43737190d0
   double precision, parameter :: c5l5 =-2.01499702d0
   double precision, parameter :: c6l5 =-0.03220585d0
   double precision, parameter :: c0m5 =-51.44581130d0
   double precision, parameter :: c1m5 =-35.53559269d0
   double precision, parameter :: c2m5 =-16.38760953d0
   double precision, parameter :: c3m5 =-4.05108269d0
   double precision, parameter :: c4m5 =-0.54278446d0
   double precision, parameter :: c5m5 =-0.03725323d0
   double precision, parameter :: c6m5 =-0.00102639d0
   double precision, parameter :: c0h5 =-21.53920974d0
   double precision, parameter :: c1h5 =-0.77956713d0
   double precision, parameter :: c2h5 =-0.00447583d0
   double precision, parameter :: c3h5 =-0.02413154d0
!  pCH4 = 1e-4 bar
   double precision, parameter :: c0l4 = 28140.72396961d0
   double precision, parameter :: c1l4 = 16376.80570866d0
   double precision, parameter :: c2l4 = 3958.07245924d0
   double precision, parameter :: c3l4 = 508.70002283d0
   double precision, parameter :: c4l4 = 36.66431294d0
   double precision, parameter :: c5l4 = 1.40501860d0
   double precision, parameter :: c6l4 = 0.02236533d0
   double precision, parameter :: c0m4 =-23.30525036d0
   double precision, parameter :: c1m4 =-9.12942967d0
   double precision, parameter :: c2m4 =-7.48270512d0
   double precision, parameter :: c3m4 =-2.84047828d0
   double precision, parameter :: c4m4 =-0.51671076d0
   double precision, parameter :: c5m4 =-0.04486088d0
   double precision, parameter :: c6m4 =-0.00149768d0
   double precision, parameter :: c0h4 =-21.85614193d0
   double precision, parameter :: c1h4 =-0.92312602d0
   double precision, parameter :: c2h4 =-0.05625981d0
   double precision, parameter :: c3h4 =-0.03559932d0
!  pCH4 = 1e-3 bar
   double precision, parameter :: c0l3 =-52833.30554892d0
   double precision, parameter :: c1l3 =-30537.38407390d0
   double precision, parameter :: c2l3 =-7335.27739995d0
   double precision, parameter :: c3l3 =-937.13019049d0
   double precision, parameter :: c4l3 =-67.16841179d0
   double precision, parameter :: c5l3 =-2.56125463d0
   double precision, parameter :: c6l3 =-0.04059850d0
   double precision, parameter :: c0m3 = 60.83212721d0
   double precision, parameter :: c1m3 = 99.29765579d0
   double precision, parameter :: c2m3 = 48.25740585d0
   double precision, parameter :: c3m3 = 11.72742599d0
   double precision, parameter :: c4m3 = 1.52684085d0
   double precision, parameter :: c5m3 = 0.10163283d0
   double precision, parameter :: c6m3 = 0.00271414d0
   double precision, parameter :: c0h3 =-21.87862669d0
   double precision, parameter :: c1h3 =-0.51388277d0
   double precision, parameter :: c2h3 = 0.31136680d0
   double precision, parameter :: c3h3 = 0.03329049d0
!  pCH4 = 2e-3 bar
   double precision, parameter :: c0l23 = 33356.67942747d0
   double precision, parameter :: c1l23 = 18811.76791819d0
   double precision, parameter :: c2l23 = 4411.95131782d0
   double precision, parameter :: c3l23 = 551.11718816d0
   double precision, parameter :: c4l23 = 38.67404407d0
   double precision, parameter :: c5l23 = 1.44556441d0
   double precision, parameter :: c6l23 = 0.02248493d0
   double precision, parameter :: c0m23 = 47.51821659d0
   double precision, parameter :: c1m23 = 87.21966045d0
   double precision, parameter :: c2m23 = 44.52985084d0
   double precision, parameter :: c3m23 = 11.32559623d0
   double precision, parameter :: c4m23 = 1.53947964d0
   double precision, parameter :: c5m23 = 0.10678413d0
   double precision, parameter :: c6m23 = 0.00296709d0
   double precision, parameter :: c0h23 =-22.06530949d0
   double precision, parameter :: c1h23 =-0.97218386d0
   double precision, parameter :: c2h23 = 0.10592109d0
   double precision, parameter :: c3h23 = 0.00207026d0

!  Byrne and Goldblatt (2014)
   double precision, parameter :: m1 = 1173d0
   double precision, parameter :: m2 = 71636d0
   double precision, parameter :: m3 = 0.824d0
   double precision, parameter :: m4 = 0.8d0
   double precision, parameter :: m5 = 0.2d0

   double precision, parameter :: rho = 1025d0 ! Density of seawater [kg/m3]
   double precision, parameter :: Sal = 35d0   ! Salinity
   double precision, parameter :: Vocn = 1.38d18 ! Ocean volume [m3]
   double precision, parameter :: ncal = 1.7d0

   double precision, parameter :: Kco2  = 0.06d0
   double precision, parameter :: Khco3 = 0.05d0

!  f_H2O vs Temperature (Wolf et al., 2017)
   double precision, parameter :: fesc0 = 198.34d0
   double precision, parameter :: fesc1 =-1.6856d0
   double precision, parameter :: fesc2 = 4.147d-3
   double precision, parameter :: fesc3 =-2.7947d-6
   double precision, parameter :: fesc = 2.5d13/3.74d-3 ! [mol/yr]
   double precision, parameter :: MocnH2O = 7.8d22 ! [mol]

   double precision, parameter :: cfeox0 = 0.9713d0
   double precision, parameter :: cfeox1 =-0.1095d0
   double precision, parameter :: cfeox2 =-0.0737d0

 END MODULE Constants
