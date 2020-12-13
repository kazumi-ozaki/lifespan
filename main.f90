    Program LIFESPAN
!-------------------------------------------------------------------------------------------------------!
!-------------------------------------------------------------------------------------------------------!
!                                                                                                       !
!   main.f90                                                                                            !
!   Constants.f90                                                                                       !
!                                                                                                       !
!   2020/11/07    Kazumi Ozaki                                                                          !
!                                                                                                       !
!   Department of Environmental Science                                                                 !
!   Toho University                                                                                     !
!   E-mail: kazumi.ozaki@sci.toho-u.ac.jp                                                               !
!                                                                                                       !
!-------------------------------------------------------------------------------------------------------!
!   It is advisable to read the README.txt                                                              !
!-------------------------------------------------------------------------------------------------------!
!
    USE Constants ! Summarizing constant values used in this model (see Constants.f90).
!
    Implicit None
!
!-----------------------!
! Variable declaration  ! See also XXX.xlsx for the meaning of each variable.
!-----------------------!
  ! General !
    Integer:: I,max,Icount,Icount2,Icount3,Ij,Ij2,Ioutput,Imc,Ierror
    Integer:: Ianimal,Ioae,I1pPAL,Iozone,judge,Ihuman,Ihuman2,Ihuman3
    Integer:: Ic1pPAL,Ic10pPAL,Ichuman,Ichuman2,Ichuman3,Isucc,Isuc2
    Parameter(max = 20e8)
    double precision:: age,dt,Time,sTime,t1,t2,dammy
  ! Reservoir !
    double precision:: P(2),N(2),O(2),A(2),G(2),C(2),S(2),PYR(2),GYP(2),CAL(2),ALK(2)
    double precision:: P_success(600),N_success(600),O_success(600),A_success(600)
    double precision:: G_success(600),C_success(600),S_success(600),PYR_success(600)
    double precision:: GYP_success(600),CAL_success(600),ALK_success(600)
    double precision:: P0ma,N0ma,O0ma,A0ma,G0ma,C0ma,S0ma,PYR0ma,GYP0ma,CAL0ma
    double precision:: pCO2(2),pCO2bar,pCO2c,RCO2,pCO20ma,mCO2(2),RCO2_success(600)
    double precision:: pO2(2),pO20ma,mO2(2),mO20ma
    double precision:: pCH4(2),pCH4c,Mch4(2),Mch40,Mch40ma
    double precision:: Patm,m_CH4(2),Matm_mol,Matm_CO2,Patm_success(600)
! Flux !
    double precision:: Rp,Wp,Bp,Bporg,Bpfe,Bpca,Fnfix,Fdeni,Bnorg,Bcorg,Btcorg
    double precision:: Worg,Fmorg,Wsil,Wcarb,Fmcarb,Bcarb,Bpyr,Wpyr,Bgyp,Wgyp
    double precision:: Rp_success(600),Wp_success(600),Bporg_success(600),Bpfe_success(600),Bpca_success(600)
    double precision:: Fnfix_success(600),Fdeni_success(600),Bnorg_success(600)
    double precision:: Bcorg_success(600),Btcorg_success(600),Worg_success(600),Fmorg_success(600)
    double precision:: Wsil_success(600),Wcarb_success(600),Fmcarb_success(600),Bcarb_success(600)
    double precision:: Bpyr_success(600),Wpyr_success(600),Bgyp_success(600),Wgyp_success(600)
    double precision:: CNorg,CPorg
    double precision:: delAc(2),delAtmc(2),delOc(2),delGc(2),delCc(2)
    double precision:: DelC,DelP,delAs(2),delPYRs(2),delGYPs(2),DelS
    double precision:: d34S(2),d34Spyr(2),d13C(2),d13Corg(2)
    double precision:: d34S_success(600),d34Spyr_success(600),d13C_success(600),d13Corg_success(600)
! Forcing !
    double precision:: fG,fR,fP,fE,fC,fpP
    double precision:: G1,G2,R1,R2,P1,P2,E1,E2,C1,C2,PP1,PP2
    double precision:: tfe1,tfe2,tfp1,tfp2,tfpP1,tfpP2
    double precision:: fG_success(600),fR_success(600),fP_success(600),fE_success(600),fC_success(600),fpP_success(600)
! Climate !
    double precision:: Tc,Tk,Teff,dTk,albedo,Sun,psi
    double precision:: TOA,poly,f1,f2,f3,Tk1,Tk2,Tk3,pln
    double precision:: Sun_success(600),albedo_success(600),TOA_success(600),jOLR_success(600),radforce_CH4_success(600)

    double precision:: V,Vnpp,VnppO2,VnppCO2,VnppT,VnppCO2_c3,VnppCO2_c4
    double precision:: V_success(600),Vnpp_success(600),VnppO2_success(600),VnppCO2_success(600),VnppT_success(600)
    double precision:: ignit_success(600),fire_success(600)
    double precision:: EpsCO2,EpsO2
    double precision:: fco2,gco2,fpre,fplant,mvw,fT,gpre,gplant,gT
    double precision:: L,CPland,Pland
    double precision:: Po,DOA,fC0,DOA_success(600)
    double precision:: fo2,fso4,fch4,Jch4t
    double precision:: PSIch4,psiO2,PoL,Frem,fco2marine
    double precision:: go2,gch4,Fremt,NPPocean,NPPland,VnppCO2_ocean,gamma,delta
    double precision:: kch4ox,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23
    double precision:: phi_o2,phi_ch4,Mo2_ox,Mch4_ox
    double precision:: fo2_success(600),fch4_success(600),go2_success(600),gch4_success(600),Jch4t_success(600)
! Monte Carlo analysis !
    double precision:: nG,mG,aG,tauG,aR,tauR
    double precision:: JsbdCorg,JsbdSpyr,JsbdFeox,JmanC,JmanS,JmanRed,JsbdCarb,JsbdSgyp
    double precision:: JsbdCorg0,JsbdSpyr0,JsbdFeox0,JsbdCarb0,JsbdSgyp0,JmanC0,JmanS0,JmanRed0
    double precision:: fUV,Tref,Tref_land
    double precision:: LIFE,FERT,ACT,Pmin,CPanoxic,dOLR,dTOA
    double precision:: nG_rnd(resample),mG_rnd(resample)
    double precision:: aG_rnd(resample),tauG_rnd(resample),aR_rnd(resample),tauR_rnd(resample)
    double precision:: JsbdCorg0_rnd(resample),JsbdSpyr0_rnd(resample),JsbdFeox0_rnd(resample)
    double precision:: JsbdCarb0_rnd(resample),JsbdSgyp0_rnd(resample)
    double precision:: JmanC0_rnd(resample),JmanS0_rnd(resample),JmanRed0_rnd(resample)
    double precision:: Corg0_rnd(resample),Carb0_rnd(resample),Spyr0_rnd(resample),Sgyp0_rnd(resample)
    double precision:: fUV_rnd(resample),Tref_rnd(resample),Tref_land_rnd(resample)
    double precision:: LIFE_rnd(resample),FERT_rnd(resample),ACT_rnd(resample)
    double precision:: Pmin_rnd(resample),Kp,RUNsil,RUNsil_rnd(resample)
    double precision:: CPanoxic_rnd(resample),dOLR_rnd(resample),dTOA_rnd(resample)

    double precision:: thuman,thuman2,thuman3,tanimal,toae,tozone,t1pPAL,dTch4
    double precision:: K0,K1,K2,KspCal
    double precision:: a_,b_,c_,H,pH,Omega,Cca,Cco3,Chco3,Cco2
    double precision:: Omega_success(600),Cco3_success(600),Chco3_success(600),Cco2_success(600)
    double precision:: fco2limit,ffco2,ffhco3
    double precision:: sFo2,sFch4,a_o2ch4,b_o2ch4,c_o2ch4
    double precision:: GRBaos,GRBsurf,GRBaos_success(600),GRBsurf_success(600)
    double precision:: Fmcarb0,Worg0,Bgyp0,Bpyr0
    double precision:: Jred_0Ma,Corg0,Carb0,Spyr0,Sgyp0
    double precision:: JsbdCorg_success(600),JsbdSpyr_success(600),JsbdFeox_success(600)
    double precision:: JmanC_success(600),JmanS_success(600),JmanRed_success(600)
    double precision:: JsbdCarb_success(600),JsbdSgyp_success(600)
    double precision:: pO2_success(600),mO2_success(600),pCH4_success(600),pCO2_success(600)
    double precision:: m_CH4_success(600),mCO2_success(600)
    double precision:: Tc_success(600),Tk_success(600),pH_success(600)
    double precision:: NPPocean_success(600),NPPland_success(600)
    double precision:: Lifespan10PAL_success,Lifespan1PAL_success,Lifespan01PAL_success
    double precision:: Lifespan18p_success,Lifespan16p_success,Lifespan10p_success
    double precision:: sumJred,sumJred_10PAL,sumJred_1PAL,sumJred_01PAL,sumJred_16p,sumJred_18p,sumJred_10p
    double precision:: aveJred_10PAL,aveJred_1PAL,aveJred_01PAL,aveJred_16p,aveJred_18p,aveJred_10p
    double precision:: sumTime,sumTime10PAL,sumTime1PAL,sumTime01PAL,sumTime16p,sumTime18p,sumTime10p
    double precision:: sumGRBexo,sumGRBexo10PAL,sumGRBexo1PAL,sumGRBexo01PAL
    double precision:: sumGRBexo16p,sumGRBexo18p,sumGRBexo10p
    double precision:: sumJman,sumJman_10PAL,sumJman_1PAL,sumJman_01PAL,sumJman_16p,sumJman_18p,sumJman_10p
    double precision:: sumJsbd,sumJsbd_10PAL,sumJsbd_1PAL,sumJsbd_01PAL,sumJsbd_16p,sumJsbd_18p,sumJsbd_10p
    double precision:: Jsbd0Ma,Jesc0Ma,Jman0Ma
    double precision:: f_H2O,Jesc_H2O,Jesc_CH4,t_ocn
    double precision:: f_H2O_success(600),Jesc_H2O_success(600),Jesc_CH4_success(600),t_ocn_success(600)
!
!------------------------!
!  Opening output files  !
!------------------------!
    Open(10,file='output/all.dat')          ! All story
    Open(11,file='output/success.dat')      ! Success story
    Open(12,file='output/failure.dat')      ! Failure story
    Open(13,file='output/present.dat')      ! Present-day condition of success stories
    Open(14,file='output/greenhouse.dat')   ! Moist greenhouse stories
    Open(15,file='output/atmosphere.dat')   ! Atmospheric composition
    Open(16,file='output/reservoir.dat')    ! Reservoir size
    Open(17,file='output/flux.dat')         ! Flux
    Open(18,file='output/etc.dat')          ! Other variables
    Open(19,file='output/GRB.dat')          ! Global redox budget
    Open(20,file='output/isotope.dat')      ! Isotopic value
    Open(21,file='output/forcing.dat')      ! Forcing
    Open(22,file='output/climate.dat')      ! Climate
    Open(23,file='output/vegetation.dat')   ! Vegetation
    Open(24,file='output/productivity.dat') ! Productivity
    Open(25,file='output/carbon_sys.dat')   ! Carbonate system
    open(26,file='output/Lifespan_18%.dat')
    open(27,file='output/Lifespan_16%.dat')
    open(28,file='output/Lifespan_10%.dat')
    open(29,file='output/Lifespan_10%PAL.dat')
    open(30,file='output/Lifespan_1%PAL.dat')
    open(31,file='output/Lifespan_01%PAL.dat')
    open(32,file='output/Lifespan_1e-6%PAL.dat')

!
!-------------------------!
! Monte Carlo simulation  !
!-------------------------!
  ! Preparation !
    Call RANDOM(nG_rnd,mG_rnd,aG_rnd,tauG_rnd,aR_rnd,tauR_rnd                          &
               ,JsbdCorg0_rnd,JsbdSpyr0_rnd,JsbdFeox0_rnd,JsbdCarb0_rnd,JsbdSgyp0_rnd  &
               ,JmanC0_rnd,JmanS0_rnd,JmanRed0_rnd                                     &
               ,Corg0_rnd,Spyr0_rnd,Carb0_rnd,Sgyp0_rnd                                &
               ,fUV_rnd,Tref_rnd,Tref_land_rnd,LIFE_rnd,FERT_rnd,ACT_rnd,Pmin_rnd      &
               ,RUNsil_rnd,CPanoxic_rnd,dOLR_rnd,dTOA_rnd)
!
    Do Imc = 1, resample !------------------ Start Monte Carlo simulation-------------------------!
!     Do Imc = 1, 1
    ! Setting up MC analysis !
      Call SETUP(Imc,JsbdCorg0,JsbdSpyr0,JsbdCarb0,JsbdSgyp0,JmanC0,JmanS0,JmanRed0   &
                ,JsbdCorg0_rnd,JsbdSpyr0_rnd,JsbdCarb0_rnd,JsbdSgyp0_rnd              &
                ,JmanC0_rnd,JmanS0_rnd,JmanRed0_rnd                                   &
                ,nG_rnd,mG_rnd,aG_rnd,tauG_rnd,aR_rnd,tauR_rnd                        &
                ,Corg0_rnd,Spyr0_rnd,Carb0_rnd,Sgyp0_rnd                              &
                ,fUV_rnd,Tref_rnd,Tref_land_rnd,LIFE_rnd,FERT_rnd                     &
                ,ACT_rnd,Pmin_rnd,RUNsil_rnd,CPanoxic_rnd,dOLR_rnd,dTOA_rnd           &
                ,Fmcarb0,Worg0,Bgyp0,Bpyr0                                            &
                ,aG,aR,tauG,tauR,nG,mG,fUV,Tref,Tref_land,LIFE,FERT,ACT               &
                ,Pmin,Kp,RUNsil,CPanoxic,dOLR,dTOA) 
    ! Opening input files
      open( 1,file = 'input/fC.dat')
      open( 2,file = 'input/fR.dat')
      open( 3,file = 'input/fG.dat')
      open( 4,file = 'input/fE.dat')
      open( 7,file = 'input/fP.dat')
      open( 8,file = 'input/fpP.dat')

      read(1,*) t1,C1 ! fC.dat
      read(1,*) t2,C2 ! fC.dat
      fC = C1
      read(2,*) R1   ! fR.dat
      read(2,*) R2   ! fR.dat
      read(3,*) G1   ! fG.dat
      read(3,*) G2   ! fG.dat
      fR = R1
      fG = G1
      read(4,*) tfe1,  E1   ! fE.dat
      read(4,*) tfe2,  E2   ! fE.dat
      read(7,*) tfp1,  P1   ! fP.dat
      read(7,*) tfp2,  P2   ! fP.dat
      read(8,*) tfpP1, PP1  ! fpP.dat
      read(8,*) tfpP2, PP2  ! fpP.dat
      fE  = E1
      fP  = P1
      fpP = PP1
      tfe1  = tfe1*1d6
      tfe2  = tfe2*1d6
      tfp1  = tfp1*1d6
      tfp2  = tfp2*1d6
      tfpP1 = tfpP1*1d6
      tfpP2 = tfpP2*1d6
      fC_success(1) = fC
      fR_success(1) = fR
      fG_success(1) = fG
      fE_success(1) = fE
      fP_success(1) = fP
      fpP_success(1) = fpP
!
    ! Initialization  !
      Call INIT(Imc,CPland,CNorg,Tk,Tc,DelP,Bcarb,f_H2O,Jesc_H2O,P,N,O,A,G,C,S,PYR,GYP,CAL,ALK   &
               ,Corg0_rnd,Carb0_rnd,Spyr0_rnd,Sgyp0_rnd,P_success,N_success,O_success,A_success  &
               ,G_success,C_success,S_success,PYR_success,GYP_success,CAL_success,ALK_success    &
               ,JsbdCorg_success,JsbdCorg0_rnd,JsbdSpyr_success,JsbdSpyr0_rnd,JsbdCarb_success   &
               ,JsbdCarb0_rnd,JsbdSgyp_success,JsbdSgyp0_rnd,JmanC_success,JmanC0,JmanS_success  &
               ,JmanS0,JmanRed_success,JmanRed0,GRBaos_success,GRBsurf_success                   &
               ,pO2_success,mO2_success,pCH4_success,pCO2_success,m_CH4_success,mCO2_success     &
               ,Tc_success,Tk_success,pH_success,NPPocean_success,NPPland_success,f_H2O_success  &
               ,Jesc_H2O_success,Jesc_CH4_success,t_ocn_success,RCO2_success                     &
               ,d34S_success,d34Spyr_success,d13C_success,d13Corg_success                        &
               ,DOA_success,Patm_success,Sun_success,albedo_success,V_success,Vnpp_success       &
               ,VnppO2_success,VnppCO2_success,VnppT_success,ignit_success,fire_success          &
               ,Omega_success,Cco3_success,Chco3_success,Cco2_success,Rp_success,Wp_success      &
               ,Bporg_success,Bpfe_success,Bpca_success,Fnfix_success,Fdeni_success              &
               ,Bnorg_success,Bcorg_success,Btcorg_success,Worg_success,Fmorg_success            &
               ,Wsil_success,Wcarb_success,Fmcarb_success,Bcarb_success,Bpyr_success             &
               ,Wpyr_success,Bgyp_success,Wgyp_success                                           &
               ,fo2_success,fch4_success,go2_success,gch4_success,Jch4t_success                  &
               ,delCc,delGc,delAtmc,delAc,delOc,delAs,delGYPs,d34S,delPYRs,d34Spyr,d13C,d13Corg  &
               ,pCO2,RCO2,pCO2c,mO2,pO2,MCH40,Mch4,Mo2_ox,Mch4_ox,phi_o2,phi_ch4,pCH4,pCH4c      &
               ,Jesc_CH4,psiO2,PSIch4,gamma,delta,fco2limit,Patm,Matm_mol                        &
               ,thuman,thuman2,thuman3,tanimal,toae,t1pPAL,tozone,Ihuman,Ihuman2,Ihuman3,Ianimal &
               ,Ioae,I1pPAL,Iozone,judge,Ichuman,Ichuman2,Ichuman3,Ic1pPAL,Ic10pPAL,Icount3      &
               ,Isucc,Isuc2,sumJred,sumTime,sumGRBexo,sumJman,sumJsbd,Time,sTime,Ij,Ij2,Ioutput  &
              ,kch4ox,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,dt,Fmcarb0,Worg0,Bgyp0,Bpyr0)
               pCO2bar = pCO2(1)*1d-6/0.987d0
               pln = dlog(pCO2(1)/0.987d0/330d0)
               TOA_success(1) = TOA_high(a00,Tk,pCO2bar,dTOA)
               jOLR_success(1) = jOLR(pln,Tk,dOLR)
               radforce_CH4_success(1) = radforce_CH4(pCH4)
!  
!---------------------------------------------------------------------------------------------------------!
!   Time Evolution                                                                                        !
!---------------------------------------------------------------------------------------------------------!
    Do  !--------------------------------- Start Time Evolution ------------------------------------------!
    ! Time measurement with a variable timestep !
      If(pO2(1) >= pO20*0.005d0) then
        dt = 10d0
        Icount = Int(1d6/dt)
        Icount2 = Int(10d6/dt)
        Ij = Ij+1
        Ij2 = Ij2+1
      Else If(pO2(1) < pO20*0.005d0) then
        dt = 0.05d0
        Icount = Int(1d6/dt)
        Icount2 = Int(10d6/dt)
        Ij = Ij+1
        Ij2 = Ij2+1
      End If
      sTime = sTime + dt
!
!----------!
! Lifespan !
!----------!
    If(sTime >= 0d0) then
    ! Lifespan >18% !
      If((Ichuman == 0).and.(mO2(1) < 0.18d0)) then
        judge = 0
        Ichuman = 1
        ! Lifespan_18%.dat
        Write(26,918) judge,-Jred_0Ma/1d12,-sumJred_18p/Lifespan18p_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jsbd0Ma/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6  &
                     ,thuman/1d6,sTime/1d6
      End If
    ! Lifespan >16% !
      If((Ichuman2 == 0).and.(mO2(1) < 0.16d0)) then
        judge = 0
        Ichuman2 = 1
        ! Lifespan_16%.dat
        Write(27,919) judge,-Jred_0Ma/1d12,-sumJred_16p/Lifespan16p_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jsbd0Ma/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6  &
                     ,thuman2/1d6,thuman/1d6,sTime/1d6
      End If
    ! Lifespan >10% !
      If((Ichuman3 == 0).and.(mO2(1) < 0.1d0)) then
        judge = 0
        Ichuman3 = 1
        ! Lifespan_10%.dat
        Write(28,920) judge,-Jred_0Ma/1d12,-sumJred_10p/Lifespan10p_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jsbd0Ma/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6  &
                     ,thuman3/1d6,thuman2/1d6,thuman/1d6,sTime/1d6
      End If
    ! Lifespan >10%PAL !
      If((Ic10pPAL == 0).and.(pO2(1) < pO20*0.1d0)) then
        judge = 0
        Ic10pPAL = 1
        ! Lifespan_10%PAL.dat
        Write(29,918) judge,-Jred_0Ma/1d12,-sumJred_10PAL/Lifespan10PAL_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jsbd0Ma/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6      &
                     ,tanimal/1d6,sTime/1d6
      End If
    ! Lifespan >1%PAL !
      If((Ic1pPAL == 0).and.(pO2(1) < pO20*1d-2)) then
        judge = 0
        Ic1pPAL = 1
        ! Lifespan_1%PAL.dat
        Write(30,918) judge,-Jred_0Ma/1d12,-sumJred_1PAL/Lifespan1PAL_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6,tanimal/1d6     &
                     ,t1pPAL/1d6,sTime/1d6
      End If
    ! Lifespan >1e-6%PAL !
      If(pO2(1) < pO20*1d-8) then
        judge = 0
        ! Lifespan_1e-6%PAL.dat
        Write(32,919) judge,-Jred_0Ma/1d12,-sumJred_01PAL/Lifespan01PAL_success/1d18,pO2(2),nG,mG,aG,aR &
                     ,tauG,tauR,fUV,JsbdCorg0/1d12,Jesc0Ma/1d12,Jman0Ma/1d12,toae/1d6,tanimal/1d6       &
                     ,t1pPAL/1d6,tozone/1d6,sTime/1d6
        Go to 9998
      End If
    End If
!
!-----------!
!  Forcing  !
!-----------!
! Update
    If(Time < -1d6) then
      If(Ij == Icount) then
        R1 = R2
        Read(2,*) R2  ! fR.dat
        G1 = G2
        Read(3,*) G2  ! fG.dat
        t1 = time
        Ioutput = 1
        Time = Time + 1d6
        Write(*,*) 'Time=',sTime/1d6
        Ij = 0
        Isucc = Isucc + 1
        If(Isucc <= 600) then
          pO2_success(Isucc) = pO2(1)
          mO2_success(Isucc) = mO2(1)
          pCH4_success(Isucc) = pCH4(1)
          pCO2_success(Isucc) = pCO2(1)
          m_CH4_success(Isucc) = m_CH4(1)
          mCO2_success(Isucc) = mCO2(1)
          P_success(Isucc) = P(1)
          N_success(Isucc) = N(1)
          O_success(Isucc) = O(1)
          A_success(Isucc) = A(1)
          G_success(Isucc) = G(1)
          C_success(Isucc) = C(1)
          S_success(Isucc) = S(1)
          PYR_success(Isucc) = PYR(1)
          GYP_success(Isucc) = GYP(1)
          CAL_success(Isucc) = CAL(1)
          ALK_success(Isucc) = ALK(1)
          JsbdCorg_success(Isucc) = JsbdCorg
          JsbdSpyr_success(Isucc) = JsbdSpyr
          JsbdFeox_success(Isucc) = JsbdFeox
          JsbdCarb_success(Isucc) = JsbdCarb
          JsbdSgyp_success(Isucc) = JsbdSgyp
          JmanC_success(Isucc) = JmanC
          JmanS_success(Isucc) = JmanS
          JmanRed_success(Isucc) = JmanRed
          GRBaos_success(Isucc) = GRBaos
          GRBsurf_success(Isucc) = GRBsurf
          Tc_success(Isucc) = Tc
          Tk_success(Isucc) = Tk
          pH_success(Isucc) = pH
          Omega_success(Isucc) = Omega
          Cco3_success(Isucc) = Cco3
          Chco3_success(Isucc) = Chco3
          Cco2_success(Isucc) = Cco2
          NPPocean_success(Isucc) = NPPocean
          NPPland_success(Isucc) = NPPland
          f_H2O_success(Isucc) = f_H2O
          Jesc_H2O_success(Isucc) = Jesc_H2O
          Jesc_CH4_success(Isucc) = Jesc_CH4
          t_ocn_success(Isucc) = t_ocn
          d34S_success(Isucc) = d34S(1)
          d34Spyr_success(Isucc) = d34Spyr(1)
          d13C_success(Isucc) = d13C(1)
          d13Corg_success(Isucc) = d13Corg(1)
          fC_success(Isucc) = fC
          fR_success(Isucc) = fR
          fG_success(Isucc) = fG
          fE_success(Isucc) = fE
          fP_success(Isucc) = fP
          fpP_success(Isucc) = fpP
          Sun_success(Isucc) = Sun
          albedo_success(Isucc) = albedo
          TOA_success(Isucc) = TOA_high(albedo,Tk,pCO2bar,dTOA)
          jOLR_success(Isucc) = jOLR(pln,Tk,dOLR)
          radforce_CH4_success(Isucc) = radforce_CH4(m_CH4)
          DOA_success(Isucc) = DOA
          Patm_success(Isucc) = Patm
          RCO2_success(Isucc) = RCO2
          V_success(Isucc) = V
          Vnpp_success(Isucc) = Vnpp
          VnppO2_success(Isucc) = VnppO2
          VnppCO2_success(Isucc) = VnppCO2
          VnppT_success(Isucc) = VnppT
          ignit_success(Isucc) = ignit(mO2)
          fire_success(Isucc) = kfire / (kfire-1d0+ignit(mO2))
          Rp_success(Isucc) = Rp
          Wp_success(Isucc) = Wp
          Bporg_success(Isucc) = Bporg
          Bpfe_success(Isucc) = Bpfe
          Bpca_success(Isucc) = Bpca
          Fnfix_success(Isucc) = Fnfix
          Fdeni_success(Isucc) = Fdeni
          Bnorg_success(Isucc) = Bnorg
          Bcorg_success(Isucc) = Bcorg
          Btcorg_success(Isucc) = Btcorg
          Worg_success(Isucc) = Worg
          Fmorg_success(Isucc) = Fmorg
          Wsil_success(Isucc) = Wsil
          Wcarb_success(Isucc) = Wcarb
          Fmcarb_success(Isucc) = Fmcarb
          Bcarb_success(Isucc) = Bcarb
          Bpyr_success(Isucc) = Bpyr
          Wpyr_success(Isucc) = Wpyr
          Bgyp_success(Isucc) = Bgyp
          Wgyp_success(Isucc) = Wgyp
          fo2_success(Isucc) = fo2
          fch4_success(Isucc) = fch4
          go2_success(Isucc) = go2
          gch4_success(Isucc) = gch4
          Jch4t_success(Isucc) = Jch4t
        End If
      End If
      If(Ij2 == Icount2) then
        C1 = C2
        Read(1,*) t2,C2  ! fC.dat
        t1 = time
        Ij2 = 0
      End If
      If(time >= tfe2) then
        E1 = E2
        read(4,*) tfe2,E2  ! fE.dat
        tfe1 = time
        tfe2 = tfe2*1d6
      End If
      If(time >= tfp2) then
        P1 = P2
        read(7,*) tfp2,P2  ! fP.dat
        tfp1 = time
        tfp2 = tfp2*1d6
      End If
      If(time >= tfpP2) then
        PP1 = PP2
        read(8,*) tfpP2,PP2  ! FpP.dat
        tfpP1 = time
        tfpP2 = tfpP2*1d6
      End If
!
      fG  = (G2-G1)/1d6*(sTime-t1) + G1                  ! fG
      fP  = (P2-P1)/(tfp2-tfp1)*(sTime-tfp1) + P1        ! fP
      fR  = (R2-R1)/1d6*(sTime-t1) + R1                  ! fR
      fE  = (E2-E1)/(tfe2-tfe1)*(sTime-tfe1) + E1        ! fE
      fC  = (C2-C1)/10d6*(sTime-t1) + C1                 ! fC
      fpP = (PP2-PP1)/(tfpP2-tfpP1)*(sTime-tfpP1) + PP1  ! fpP
!
    Else
!
      fG  = (1d0+aG+aG*dsin(2d0*pi*(stime/1d6-tauG/4d0)/tauG)) &
            *((1d0+stime/1d6/4500d0)**(-nG))**mG
      fP  = 1d0
      fR  = (1d0+aR*dsin(2d0*pi*(stime/1d6-tauR)/tauR)) &
            *((1d0+stime/1d6/4500d0)**(-nG))**mG
      fE  = 1d0
      fC  = 1d0
      fpP = 1d0
!
      If(Ij == Icount) then
        Ioutput = 1
        Time = Time + 1d6
        Write(*,*) 'Time=',Time/1d6
        Ij = 0
      End If
    End If
!
!-------------!
!  Judgement  !
!-------------!
    If(Time == 0d0) then
      !---------------------------------------------------------------------!
      ! pO2 = 19-23%, pCO2 = 150-450 ppmv, [SO4] = 10-40 mM, T = 286-290 K  !
      !---------------------------------------------------------------------!
      If((mO2(1) > 0.22d0).or.(mO2(1) < 0.20d0)                   &
        .or.(S(2)/Mocean*1d3 < 10d0).or.(S(2)/Mocean*1d3 > 40d0)  &
        .or.(mCO2(1) > 4.5d-4).or.(mCO2(1) < 1.5d-4)              &
        .or.(Tk > 290d0).or.(Tk < 286d0)) then
        write(*,*) 'False...'
        ! failure.dat
        write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                     ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                     ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                     ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                     ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        Go to 9998
      Else
        If(Isuc2 == 0) then
          write(*,*) 'Success!'
          Jsbd0Ma = JsbdCorg + 2d0*JsbdSpyr -0.25d0*JsbdFeox !
          Jesc0Ma = 0.5d0*(Jesc_H2O + Jesc_CH4)              ! [molO2/yr]
          Jman0Ma = -2d0*JmanS -JmanRed                      ! 
          Jred_0Ma = JsbdCorg + 2d0*JsbdSpyr -0.25*JsbdFeox -2d0*JmanS -JmanRed
          ! success_story.dat
          write(11,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                       ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                       ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                       ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                       ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
          Do I = 1, 600
            age = (I*1d0-601d0)/1d3
            ! atmosphere.dat
            write(15,812) age,pO2_success(I),pO2_success(I)/pO20,mO2_success(I),pCH4_success(I)  &
                         ,m_CH4_success(I),pCO2_success(I)/1d6,mCO2_success(I)                   &
                         ,pCH4_success(I)/(pCO2_success(I)/1d6),RCO2_success(I),Patm_success(I)  &
                         ,f_H2O_success(I)
            ! reservoir.dat
            write(16,821) age,P_success(I)/1d15,N_success(I)/1d15,O_success(I)/1d18,A_success(I)/1d18         &
                         ,G_success(I)/1d18,C_success(I)/1d18,S_success(I)/1d18,PYR_success(I)/1d18           &
                         ,GYP_success(I)/1d18,CAL_success(I)/1d18,ALK_success(I)/1d18,P_success(I)/Mocean*1d6 &
                         ,N_success(I)/Mocean*1d6,S_success(I)/Mocean*1d3,CAL_success(I)/Mocean*1d3,ALK_success(I)/Mocean*1d3  &
                         ,(G_success(I)+C_success(I))/1d18,(PYR_success(I)+GYP_success(I))/1d18,DOA_success(I),f_H2O_success(I)
            ! flux.dat
            write(17,835) age,JsbdCorg_success(I)/1d12,JsbdSpyr_success(I)/1d12,JsbdFeox_success(I)/1d12 &
                         ,JsbdCarb_success(I)/1d12,JsbdSgyp_success(I)/1d12,JmanC_success(I)/1d12        &
                         ,JmanS_success(I)/1d12,JmanRed_success(I)/1d12                                  &
                         ,0.5d0*Jesc_H2O_success(I)/1d12,0.5d0*Jesc_CH4_success(I)/1d12,t_ocn_success(I) &
                         ,Rp_success(I)/1d12,Wp_success(I)/1d12,Bporg_success(I)/1d12,Bpfe_success(I)/1d12 &
                         ,Bpca_success(I)/1d12,(Bporg_success(I)+Bpfe_success(I)+Bpca_success(I))/1d12   &
                         ,Fnfix_success(I)/1d12,Fdeni_success(I)/1d12,Bnorg_success(I)/1d12              &
                         ,Bcorg_success(I)/1d12,Btcorg_success(I)/1d12,(Bcorg_success(I)+Btcorg_success(I))/1d12 &
                         ,Worg_success(I)/1d12,Fmorg_success(I)/1d12,Wsil_success(I)/1d12 &
                         ,Wcarb_success(I)/1d12,Fmcarb_success(I)/1d12,Bcarb_success(I)/1d12 &
                         ,Bpyr_success(I)/1d12,Wpyr_success(I)/1d12,Bgyp_success(I)/1d12,Wgyp_success(I)/1d12 &
                         ,Jch4t_success(I)/1d12
            ! etc.dat
            write(18,812) age,Tc_success(I),Tk_success(I),pH_success(I),NPPocean_success(I)*12d0/1d15     &
                         ,NPPland_success(I)*12d0/1d15,(NPPocean_success(I)+NPPland_success(I))*12d0/1d15 &
                         ,(NPPocean_success(I)+NPPland_success(I))/(NPP0+NPPt0)                           &
                         ,fo2_success(I),fch4_success(I),go2_success(I),gch4_success(I)
            ! GRB.dat
            write(19,803) age,GRBaos_success(I)/1d12,GRBsurf_success(I)/1d12
            ! isotope.dat
            write(20,805) age,d13C_success(I),d13Corg_success(I),d34S_success(I),d34Spyr_success(I)
            ! forcing.dat
            write(21,807) age,fC_success(I),fR_success(I),fG_success(I),fE_success(I),fP_success(I),fpP_success(I)
            ! climate.dat
            write(22,808) age,Tc_success(I),Tk_success(I),Sun_success(I),albedo_success(I),TOA_success(I) &
                         ,jOLR_success(I),radforce_CH4_success(I)
            ! vegetation.dat
            write(23,808) age,V_success(I),Vnpp_success(I),VnppO2_success(I),VnppCO2_success(I) &
                         ,VnppT_success(I),ignit_success(I),fire_success(I)
            ! productivity.dat
            write(24,810) age,NPPocean_success(I)*12d0/1d15,NPPland_success(I)*12d0/1d15  &
                         ,(NPPocean_success(I)+NPPland_success(I))*12d0/1d15              &
                         ,(NPPocean_success(I)+NPPland_success(I))/(NPP0+NPPt0)           &
                         ,rCP*N_success(I)/rNP/Mocean*1d6,rCP*P_success(I)/Mocean*1d6     &
                         ,(Cco2_success(I)+Chco3_success(I))/rho*1d6                      &
                         ,(rCP*P_success(I)/Mocean*1d6)/(rCP*N_success(I)/rNP/Mocean*1d6) &
                         ,((Cco2_success(I)+Chco3_success(I))/rho*1d6)/(rCP*N_success(I)/rNP/Mocean*1d6)
            ! carbonate_sys.dat
            write(25,806) age,pH_success(I),Omega_success(I),Cco3_success(I),Chco3_success(I),Cco2_success(I)
          End Do
          Isuc2 = 1
        End If
        If(Icount3 == 0) then
          ! present.dat
          write(13,860) sTime/1d9,pO2(2),pCO2(2),pCH4(2)*1d6,P(2)/Mocean*1d6,N(2)/Mocean*1d6,O(2),DOA       &
                       ,A(2)/1d18,G(2)/1d18,C(2)/1d18,S(2)/Mocean*1d3,PYR(2)/1d18,GYP(2)/1d18               &
                       ,CAL(2)/Mocean*1d3,d13C(2),d34S(2),d13Corg(2),d34Spyr(2),Tk                          &
                       ,NPPocean*12d0/1d15,NPPland*12d0/1d15,Rp/1d12,Wp/1d12,Bporg/1d12,Bpfe/1d12,Bpca/1d12 &
                       ,Fnfix/1d12,Fdeni/1d12,Bnorg/1d12,Worg/1d12,Wcarb/1d12,Bcorg/1d12,Btcorg/1d12        &
                       ,Fmorg/1d12,Fmcarb/1d12,Wgyp/1d12,Wpyr/1d12,Bgyp/1d12,Bpyr/1d12,Wsil/1d12            &
                       ,pH,Chco3,Cco3,Cco2,Omega,ALK(2)/Vocn,GRBaos/1d12,GRBsurf/1d12,JsbdCorg/1d12         &
                       ,JsbdCarb/1d12,JsbdSpyr/1d12,JsbdSgyp/1d12,JsbdFeox/1d12,JmanC/1d12,JmanS/1d12       &
                       ,JmanRed/1d12,Jch4t/1d12,0.5d0*Jesc_H2O/1d12,0.5d0*Jesc_CH4/1d12
          Icount3 = Icount3 + 1
        End If
      End If
    End If
!
!-----------!
! Integrate !
!-----------!
    If(sTime >= 0d0) then
      sumJred = sumJred + (JsbdCorg + 2d0*JsbdSpyr -0.25d0*JsbdFeox -2d0*JmanS -JmanRed) * dt
      sumJman = sumJman + (-2d0*JmanS -JmanRed) * dt
      sumJsbd = sumJsbd + (JsbdCorg + 2d0*JsbdSpyr -0.25d0*JsbdFeox) * dt
    End If
!
!------------------!
!  Energy balance  !
!------------------!
  ! Solar constant !
    Sun = Sun0 / (1d0-0.38d0*sTime/tau) ! [W/m2]
      If(sTime <= -550d6) then
        Sun = Sun0 / (1d0-0.38d0*(-550d6)/tau)
      End If
!
    pCO2bar = pCO2(1)*1d-6/0.987d0      ! 1 bar = 0.987 atm
    psi = dlog10(pCO2bar)               ! log[pCO2(bar)]
    pln = dlog(pCO2(1)/0.987d0/330d0)   ! log[pCO2(PAL)]
!
  ! Bisection method --------------------- Start ---------------------------------------------!
    If(sTime == -600d6) then
      Tk1 = 273d0                       ! Initial value for the lower bound
      Tk2 = 355d0                       ! Initial value for the upper bound
    else
      Tk1 = Tk + 10d0
      Tk2 = Tk - 10d0
    End If
    f3 = 10d0
    Ierror = 0
    Do While(dabs(f3) > 1d-3)
      Ierror = Ierror + 1
      If(Ierror > 1000) then
        write(*,*) 'Ierror'
        judge = 1
        If(Time > 0d0) then
          ! greenhouse.dat
          write(14,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                       ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                       ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                       ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                       ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        End If
          ! failure.dat
          write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                       ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                       ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                       ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                       ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
          Go to 9998
        End If
        Tk3 = (Tk1+Tk2)/2d0
      ! Lower Temp
        albedo = albedo_T(Tk1)
        If(Tk1 <= 280d0) then
          TOA = TOA_low(albedo,Tk1,pCO2bar,dTOA)
        Else
          TOA = TOA_high(albedo,Tk1,pCO2bar,dTOA)
        End If
        ! Energy balance check
        f1 = (1d0-TOA)*Sun/4d0 - jOLR(pln,Tk1,dOLR) + radforce_CH4(m_CH4)
      ! Higher Temp
        albedo = albedo_T(Tk2)
        If(Tk2 <= 280d0) then
          TOA = TOA_low(albedo,Tk2,pCO2bar,dTOA)
        Else
          TOA = TOA_high(albedo,Tk2,pCO2bar,dTOA)
        End If
        ! Energy balance check
        f2 = (1d0-TOA)*Sun/4d0 - jOLR(pln,Tk2,dOLR) + radforce_CH4(m_CH4)
      ! Centered Temp
        albedo = albedo_T(Tk3)
        If(Tk3 <= 280d0) then
          TOA = TOA_low(albedo,Tk3,pCO2bar,dTOA)
        Else
          TOA = TOA_high(albedo,Tk3,pCO2bar,dTOA)
        End If
        ! Energy balance check
        f3 = (1d0-TOA)*Sun/4d0 - jOLR(pln,Tk3,dOLR) + radforce_CH4(m_CH4)
      ! Updating
        If(f1*f3 < 0d0) then
          Tk2 = Tk3
        Else If(f2*f3 < 0d0) then
          Tk1 = Tk3
        End If
        If((f1*f3 > 0d0).and.(f2*f3 > 0d0)) then
          write(*,*) 'Error!!: ff > 0'
          write(*,*) f1,f2,f3
          write(*,*) Ierror,Tk1,Tk2,Tk3
          If(Time > 0d0) then
            ! greenhouse.dat
            write(14,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                         ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                         ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                         ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                         ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        End If
        judge = 2
        ! failure.dat
        write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                     ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                     ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                     ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                     ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        Go to 9998
      End If
    End Do
  ! Bisection method ----------------------- End ----------------------------------------------!
      Tk = Tk3
      If(Tk <= 270d0) then
        write(*,*) 'T < 270K!: ',Tk
      Else If(Tk >= 355d0) then
        judge = 2
        If(Time > 0d0) then
          ! greenhouse.dat
          write(14,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                       ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                       ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                       ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                       ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        End If
        ! failure.dat
        write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                     ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                     ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                     ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                     ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
        Go to 9998
      End If
      Tc = Tk - 273.15d0
!
!----------------------------------------------------------------------!
! Abiotic O2 production via H2O photolysis based on Wold et al. (2017) !
!----------------------------------------------------------------------!
  ! Mixing ratio of H2O in the stratosphere
    f_H2O = 10d0**(fesc0 + fesc1*Tk + fesc2*Tk*Tk + fesc3*Tk**3d0)
  ! H escape rate via H2O photolysis
    Jesc_H2O = fesc * f_H2O ! [molH2/yr]
  ! Timescale of ocean loss
    t_ocn = MocnH2O/Jesc_H2O ! [yr]
!
!------------------!
!  Land biosphere  !
!------------------!
  ! O2-effect
    VnppO2 = 1.5d0 - 0.5d0*O(1)/O0
  ! CO2-effect
    VnppCO2= (pCO2(1)-Pmin) / (Kp+pCO2(1)-Pmin)
      If(VnppCO2 < 0d0) then
        VnppCO2 = 0d0
      End If
  ! Temperature-effect
    VnppT = 1d0 - ((Tc-25d0)/Tref_land)**2d0
      If(VnppT <= 0d0) then
        VnppT = 0d0
      End If
  ! NPP w/o fire-effect
    Vnpp = 2d0 * fP * VnppO2 * VnppCO2 * VnppT
  ! Land biomass with fire-effect
    V = Vnpp * kfire / (kfire-1d0+ignit(mO2))
  ! Landbiomass with UV-effect
    V = V * dtanh(pO2(1)/pO20/fUV)
  ! NPP of terrestrial biosphere (NPPt0 = 5000 TmolC/yr)
    NPPland = NPPt0 * V
!
!-----------------------------------!
!  Weathering and debassing fluxes  !
!-----------------------------------!
  ! CO2 degassing flux via carbonate metamorphism
    Fmcarb = Fmcarb0 * fG * fC * C(1)/C0
  ! CO2 degassing flux from organic C metamorphism
    Fmorg = Fmorg0 * fG * G(1)/G0
  ! Mantle degassing
    JmanC = JmanC0 * fG     ! CO2
    JmanS = JmanS0 * fG     ! H2S
    JmanRed = JmanRed0 * fG ! Reducing gases (e.g., H2)
  ! Silicate weathering
      mvw = V * fE
      If(mvw >= 1d0) then
        mvw = 1d0
      End If
      fT     = dexp(ACT*(Tc-Tc0)) * (1d0+RUNsil*(Tc-Tc0))**0.65d0
      fpre   = fT * Rco2**0.5d0
      fplant = fT * (2d0*Rco2/(1+Rco2))**FERT
      fco2   = fpre*(1d0-mvw) + fplant*mvw
    Wsil = Wsil0 * fR * fco2 * (LIFE+(1d0-LIFE)*fE*V)
  ! Carbonate weathering
      gT     = 1d0 + RUNcarb*(Tc-Tc0)
      gpre   = gT * Rco2**0.5d0
      gplant = gT * (2d0*Rco2/(1d0+Rco2))**FERT
      gco2   = gpre*(1d0-mvw) + gplant*mvw
    Wcarb = Wcarb0 * fR * gco2 * (LIFE+(1d0-LIFE)*fE*V)
  ! Oxidative weatheirng of organic C
    Worg  = Worg0 * fR * G(1)/G0 * (O(1)/O0)**0.5d0
  ! Gypsum weathering
    Wgyp  = Wgyp0 * GYP(1)/GYP0 * Wcarb/Wcarb0
  ! Oxidative weatherign of pyrite S
    Wpyr  = Wpyr0 * fR * PYR(1)/PYR0 * (O(1)/O0)**0.5d0
  ! Phosphorus weathering
    Wp = Wp0 * (2d0/12d0*Wsil/Wsil0+5d0/12d0*Wcarb/Wcarb0+5d0/12d0*Worg/Worg0) * fpP
  ! P cycle on land
    L = k11 * V        ! Fraction of P to terrestrial biosphere
    Pland = L * Wp     ! P flux to terrestrial biosphere
    Rp = (1d0-L) * Wp  ! P flux to the ocean
    If((sTime >= -465d6).and.(sTime <= -445d6)) then
      CPland = CPland0 * (1d0+(sTime+465d6)/(-445d6+465d6))
    Else If ((sTime > -445d6).and.(sTime <= -287d6)) then
      CPland = 2d0 * CPland0
    Else
      CPland = CPland0
    End If
    BtCorg = CPland * Pland  ! Corg burial on land (= C/P x Pflux)
  ! CH4 flux from the terrestrial biosphere to the atmosphere
    Jch4t = Jch4t0 * (BtCorg/BtCorg0) * a_ch4 * dexp(b_ch4*Tc/Tc0) ! Jch4t0 = 1 Tmol/yr
!
!---------------!
!  Ocean model  !
!---------------!
  ! New production [umolC/kg]
    Po = rCP * N(1)/rNP / Mocean*1d6   ! N-limited productivity [umolC/kg]
      If(N(1)/rNP >= P(1)) then
        Po = rCP * P(1) / Mocean*1d6   ! P-limited productivity
      End If
      If(((Cco2+Chco3)/rho*1d6) <= Po) then
        Po = (Cco2+Chco3) / rho*1d6    ! CO2-limited productivity
      End If
    Po = Po * const2 * fTmarine(Tc,Tref)    ! Temperature effect
  ! Oceanic NPP (NPP0 = 3750 TmolC/yr; Po0 = 225.96)
    NPPocean = NPP0 * (Po/Po0)
  ! Degree of Anoxia
    DOA = 1d0 - (1d0-DOA0)*(O(1)/O0)*(Po0/Po)
      If(DOA <= 0d0) then
        DOA = 0d0
      End If
      If(DOA >= 1d0) then
        DOA = 1d0
      End If
  ! Nitrogen fixation [molN/yr]
    Fnfix = Fnfix0 * ((P(1)-N(1)/rNP)/(P0-N0/rNP))**2d0
      If(Fnfix <= 0d0) then
        Fnfix = 0d0
      End If
  ! Denitrification [molN/yr]
    Fdeni = Fdeni0 * (1d0+DOA/DOA0) / 2d0
  ! Corg burial [molC/yr]
    Bcorg = Bcorg0 * (Po/Po0)**2d0
  ! Corg remineraliztion
    Frem  = NPPocean - Bcorg   ! in the ocean
    Fremt = NPPland  - Btcorg  ! on the land
    fo2   = gamma              ! 
    fch4  = 1d0 - fo2          ! 
    If((NPPland == 0d0).or.(BtCorg == 0d0)) then
      gch4 = 0
    Else
      gch4 = 2d0 * Jch4t / ((NPPland-BtCorg)*(1d0-delta))
    End If
    If(gch4 > 1d0) then
      gch4 = 1d0
      Jch4t = ((NPPland-BtCorg)*(1d0-delta))*0.5d0
    End If
    If(gch4 < 0d0) then
      gch4 = 0d0
    End If
    go2 = 1d0 - gch4
  ! Norg burial [molN/yr]
    Bnorg = Bcorg / CNorg
  ! Corg/Porg ratio of buried sediments
    CPorg = CPoxic * CPanoxic / ((1d0-DOA)*CPanoxic+DOA*CPoxic)
  ! P burial flux [molP/yr]
    Bporg = Bcorg / CPorg                  ! Organic P
    Bpfe  = Bpfe0 * (1d0-DOA) / (1d0-DOA0) ! Fe-bound P
    Bpca  = Bpca0 * (Po/Po0)**2d0          ! Authigenic P
    Bp    = Bporg + Bpfe + Bpca            ! Total burial flux
  ! Pyrite burial flux [molS/yr]
    Bpyr = Bpyr0 * S(1)/S0 * Bcorg/Bcorg0 * O0/O(1)
  ! Gypsum deposition flux [molS/yr]
    Bgyp = Bgyp0 * S(1)/S0 * CAL(1)/CAL0
  ! Corg subduction flux [molC/yr]
    JsbdCorg = JsbdCorg0 * Bcorg/Bcorg0 * fG
  ! Spyr subduction flux [molS/yr]
    JsbdSpyr = JsbdSpyr0 * Bpyr/Bpyr0 * fG
  ! Sgyp subduction flux [molS/yr]
    JsbdSgyp = JsbdSgyp0 * Bgyp/Bgyp0 * fG
  ! Fe(OH)3 subduction flux [molFe/yr]
    JsbdFeox = JsbdFeox0 * fG * (1d0-DOA) / (1d0-DOA0)
  ! CaCO3 subduction flux [molC/yr]
    JsbdCarb = JsbdCarb0 * Bcarb/Bcarb0 * fG
!
! Global Redox Budget [mol O2 equiv./yr]
    GRBaos  =  0.5d0*Jesc_CH4 + 0.5d0*Jesc_H2O + Bcorg + Btcorg  &
             + 2d0*Bpyr - Worg - Fmorg - 2d0*Wpyr                &
             - 2d0*JmanS - JmanRed - 0.25d0*JsbdFeox
    GRBsurf =  0.5d0*Jesc_CH4 + 0.5d0*Jesc_H2O + JsbdCorg        &
             + 2d0*JsbdSpyr - 0.25d0*JsbdFeox - 2d0*JmanS - JmanRed
!
!---------------!
! Mass balance  !
!---------------!
  ! PO4
    P(2) = P(1) + dt*(Rp - Bp)
  ! NO3
    N(2) = N(1) + dt*(Fnfix - Fdeni - Bnorg)
  ! O2 & CH4
    sFo2 =  NPPocean - fo2*Frem -delta*fch4*Frem    &
          + NPPland - go2*Fremt - delta*gch4*Fremt  &
          - Worg - Fmorg + 2d0*Bpyr - 2d0*Wpyr      &
          - 0.5d0*Jesc_CH4 + 0.5d0*Jesc_H2O         &
          - 2d0*JmanS - JmanRed - 0.25d0*JsbdFeox
    sFch4 =  0.5d0*fch4*Frem - 0.5d0*delta*fch4*Frem    &
           + 0.5d0*gch4*Fremt - 0.5d0*delta*gch4*Fremt  &
           - 0.5d0*Jesc_CH4
    ! Implicit scheme for the photochemical reaction
      ! Reaction rate (Claire et al., 2006 Geobiology)
      Call PHOTOCHEM(pO2,pCH4,phi_o2,phi_ch4,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,kch4ox)
      a_o2ch4 = dt*kch4ox
      b_o2ch4 = 1d0-(O(1)+dt*sFo2)*dt*kch4ox+2d0*dt*kch4ox*(Mch4(1)+dt*sFch4)
      c_o2ch4 = -O(1)-dt*sFo2
      O(2) = (-b_o2ch4+dsqrt(b_o2ch4**2d0-4d0*a_o2ch4*c_o2ch4))/(2d0*a_o2ch4)
      Mch4(2) = (Mch4(1)+dt*sFch4)/(1d0+dt*kch4ox*O(2))
  ! Respiration pathway
    gamma = O(2) / (O(2)+dr)
    delta = O(2) / (O(2)+dd)
  ! Inorganic C
    A(2) = A(1) + dt*(Worg + Fmorg + Wcarb + Fmcarb - Bcarb                             &
                     - NPPocean + fo2*Frem + 0.5d0*fch4*Frem + 0.5d0*delta*fch4*Frem    &
                     - NPPland + go2*Fremt + 0.5d0*gch4*Fremt + 0.5d0*delta*gch4*Fremt  &
                     + kch4ox*O(1)*Mch4(1) + 0.5d0*Jesc_CH4 + JmanC)
  ! Crustal organic C
    G(2) = G(1) + dt*(Btcorg + Bcorg - Worg - Fmorg - JsbdCorg)
    If(G(2) <= 0d0) then
       G(2) = 0d0
    End If
  ! Crustal CaCO3
    C(2) = C(1) + dt*(Bcarb - Wcarb - Fmcarb - JsbdCarb)
    If(C(2) <= 0d0) then
      C(2) = 0d0
    End If
  ! Seawater SO4
    S(2) = S(1) + dt*(Wgyp + Wpyr - Bgyp - Bpyr + JmanS)
    If(S(2) <= 0d0) then
      S(2) = 0d0
    End If
  ! Crustal pyrite
    PYR(2) = PYR(1) + dt*(Bpyr - Wpyr - JsbdSpyr)
    If(PYR(2) <= 0d0) then
      PYR(2) = 0d0
    End If
  ! Crustal gypsum
    GYP(2) = GYP(1) + dt*(Bgyp - Wgyp - JsbdSgyp)
    If(GYP(2) <= 0d0) then
      GYP(2) = 0d0
    End If
  ! Ca2+ [mol]
    CAL(2) = CAL(1) + dt*(Wsil + Wcarb + Wgyp - Bcarb - Bgyp)
    If(CAL(2) <= 0d0) then
      CAL(2) = 0d0
    End If
    Cca = CAL(2) / Vocn ! [mol/m3]
  ! Alkalinity
    ALK(2) = ALK(1) + dt*2d0*(Wsil + Wcarb - Bcarb)
  ! Carbonate system
    K0 = dexp(lnK0(Tk))          ! CO2 solubility
    K1 = 10d0**logK1(Tk)         ! K1 [mol/kg]
    K2 = 10d0**logK2(Tk)         ! K2 [mol/kg]
    KspCal = 10d0**lnKspCal(Tk)  ! KspCal [mol/kg] [Pilson, 1998]
    a_ = ALK(2)/(K1*K2) * (1d0+Matm_mol/(Mocean*K0*Patm))
    b_ = (ALK(2)-A(2)) / K2
    c_ = ALK(2) - 2d0*A(2)
    H = (-b_+dsqrt(b_*b_-4d0*a_*c_)) / (2d0*a_) ! [H+] [mol/kg]
    pH = -dlog10(H)                       ! pH
    Cco3 = ALK(2)/Mocean / (2d0+H/K2)*rho ! [CO32-] [mol/m3]
    Chco3 = Cco3/rho * H/K2*rho           ! [HCO3-] [mol/m3]
    Cco2 = Cco3/rho * H*H/(K1*K2)*rho     ! [CO2]aq [mol/m3]
    pCO2(2) = Cco2/rho / K0*1d6           ! pCO2 [uatm]
    RCO2 = pCO2(2) / pCO20                ! RCO2 [uatm/uatm]
    ffco2 = Cco2 / (Cco2+Chco3)
    ffhco3= Chco3 / (Cco2+Chco3)
    fco2limit = (Cco2+Chco3) / (Cco2+Chco3+Kco2*ffco2+Khco3*ffhco3)
    Omega = Cca * Cco3 / (rho*rho*KspCal) ! Degree of CaCO3 saturation
  ! CaCO3 burial
    Bcarb = 3d12 * (Omega-1d0)**ncal
  ! Atmospheric composition
    a_ = const * gmolCO2
    b_ = const * (Matm_N20*gmolN2+O(2)*gmolO2+Matm_Ar0*gmolAr+Mch4(2)*gmolCH4) - pCO2(2)/1d6
    c_ = -(Matm_N20+O(2)+Matm_Ar0+Mch4(2)) * pCO2(2)/1d6
    Matm_CO2 = (-b_+dsqrt(b_*b_-4d0*a_*c_)) / (2d0*a_)         ! [mol]
    Matm_mol = Matm_N20 + O(2) + Matm_Ar0 + Matm_CO2 + Mch4(2) ! [mol]
    mCO2(2)  = Matm_CO2 / Matm_mol  ! fCO2 [mixing ratio]
    Patm = pCO2(2) / (mCO2(2)*1d6)  ! Atmospheric pressure [uatm/ppmv] = [atm]
    mO2(2) = O(2) / Matm_mol        ! fO2 [mixing ratio]
    pO2(2) = mO2(2) * Patm          ! pO2 [atm]
    m_CH4(2)= Mch4(2) / Matm_mol    ! fCH4 [mixing ratio]
    pCH4(2)= m_CH4(2) * Patm        ! pCH4 [atm]
    Jesc_CH4 = 2d0*fesc*m_CH4(2)    ! Hydrogen escape via CH4 photolysis [molH2/yr]
    pCH4c  = pCH4(2)
    psiO2  = dlog10(pO2(2)*(143.818d0+38d0)*1d18)
    PSIch4 = 10d0**(a1ch4*psiO2**4d0+a2ch4*psiO2**3d0+a3ch4*psiO2**2d0+a4ch4*psiO2+a5ch4)
    ! judge
    If(isnan(pO2(2)) .eqv. .true.) then
      judge = 4
      write(*,*) 'NaN!'
      ! failure.dat
      write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                   ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                   ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                   ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                   ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
      Go to 9998
    End If
    If((sTime < 0d0).and.(pO2(1) < 1d-5)) then
      ! failure.dat
      write(12,829) nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                   ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)           &
                   ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)    &
                   ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)             &
                   ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
      Go to 9998
    End If
!
!-----------------!
! Isotopic values !
!-----------------!
  ! d13C of an ocean-atmosphere system
    delAc(2) = delAc(1) + dt*((delGc(1)-delAc(1))*(Worg+Fmorg)    &
                             +(delCc(1)-delAc(1))*(Wcarb+Fmcarb)  &
                             -(delatmc(1)-DelP-delAc(1))*BtCorg   &
                             -(d13C(1)-DelC-delAc(1))*Bcorg       &
                             -(d13C(1)-delAc(1))*Bcarb)/A(1)
  ! d13C of crustal organic C
    delGc(2) = delGc(1) + dt*((delAtmc(1)-DelP-delGc(1))*Btcorg+(d13C(1)-DelC-delGc(1))*Bcorg)/G(1)
  ! d13C of crustal carbonate
    delCc(2) = delCc(1) + dt*((d13C(1)-delCc(1))*Bcarb)/C(1)
  ! 
    delOc(2) = delAc(2) + phi*(9483d0/Tk-23.89d0)
    delAtmc(2) = delAc(2) + (phi-1d0) * (9483d0/Tk-23.89d0)
    d13C(2) = delOc(2) + 15.10d0 - 4232d0/Tk + 0.9d0
  ! Isotopic fractionation
    EpsCO2 = -9d0 / RCO2**0.5d0
    EpsO2 = J * (O(2)/O0-1d0)
    DelC = DelCmax+EpsCO2+EpsO2
  ! d13C of burying organic C
    d13Corg(2) = d13C(2) - DelC
  ! d34S of seawater sulfate
    d34S(2) = d34S(1) + dt*((delGYPs(1)-d34S(1))*Wgyp+(delPYRs(1)-d34S(1))*Wpyr+DelS*Bpyr)/S(1)
  ! d34S of crustal pyrite
    delPYRs(2) = delPYRs(1) + dt*(d34S(1)-DelS-delPYRs(1))*Bpyr/PYR(1)
  ! d34S of crustal gypsum
    delGYPs(2) = delGYPs(1) + dt*(d34S(1)-delGYPs(1))*Bgyp/GYP(1)
  ! Isotopic fractionation
    DelS = DelS0
  ! d34S of burying pyrite
    d34Spyr(2) = d34S(2) - DelS
!
!----------!
! Lifespan !
!----------!
    If((sTime >= 0d0).and.(Ihuman3 == 0).and.(mO2(2) < 0.1d0)) then
      thuman3 = sTime
      Ihuman3 = Ihuman3 + 1
      Lifespan10p_success = sTime/1d6
      sumTime10p = sumTime/1d6
      sumJred_10p = sumJred
      sumGRBexo10p = sumGRBexo
      sumJman_10p = sumJman
      sumJsbd_10p = sumJsbd
    End If
    If((sTime >= 0d0).and.(Ihuman2 == 0).and.(mO2(2) < 0.16d0)) then
      thuman2 = sTime
      Ihuman2 = Ihuman2 + 1
      Lifespan16p_success = sTime/1d6
      sumTime16p = sumTime/1d6
      sumJred_16p = sumJred
      sumGRBexo16p = sumGRBexo
      sumJman_16p = sumJman
      sumJsbd_16p = sumJsbd
    End If
    If((sTime >= 0d0).and.(Ihuman == 0).and.(mO2(2) < 0.18d0)) then
      thuman = sTime
      Ihuman = Ihuman + 1
      Lifespan18p_success = sTime/1d6
      sumTime18p = sumTime/1d6
      sumJred_18p = sumJred
      sumGRBexo18p = sumGRBexo
      sumJman_18p = sumJman
      sumJsbd_18p = sumJsbd
    End If
    If((sTime >= 0d0).and.(Ianimal == 0).and.(pO2(2) < 0.1d0*pO20)) then
      tanimal = sTime
      Ianimal = Ianimal + 1
      Lifespan10PAL_success = sTime/1d6
      sumTime10PAL = sumTime/1d6
      sumJred_10PAL = sumJred
      sumGRBexo10PAL = sumGRBexo
      sumJman_10PAL = sumJman
      sumJsbd_10PAL = sumJsbd
    End If
    If((sTime >= 0d0).and.(Ioae == 0).and.(DOA > 0.9d0)) then
      toae = sTime
      Ioae = Ioae +1
    End If
    If((sTime >= 0d0).and.(I1pPAL == 0).and.(pO2(2) < 0.01d0*pO20)) then
      t1pPAL = sTime
      I1pPAL = I1pPAL +1
      Lifespan1PAL_success = sTime/1d6
      sumTime1PAL = sumTime/1d6
      sumJred_1PAL = sumJred
      sumGRBexo1PAL = sumGRBexo
      sumJman_1PAL = sumJman
      sumJsbd_1PAL = sumJsbd
    End If
    If((sTime >= 0d0).and.(Iozone == 0).and.(pO2(2) < 0.001d0*pO20)) then
      tozone = sTime
      Iozone = Iozone +1
      Lifespan01PAL_success = sTime/1d6
      sumTime01PAL = sumTime/1d6
      sumJred_01PAL = sumJred
      sumGRBexo01PAL = sumGRBexo
      sumJman_01PAL = sumJman
      sumJsbd_01PAL = sumJsbd
    ! Lifespan_01%PAL.dat
      write(31,748) Imc,nG,mG,aG,aR,tauG,tauR,fUV,Corg0_rnd(Imc),Carb0_rnd(Imc),Spyr0_rnd(Imc),Sgyp0_rnd(Imc) &
                   ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdFeox0_rnd(Imc),JsbdCarb0_rnd(Imc)               &
                   ,JsbdSgyp0_rnd(Imc),JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc),Tref_rnd(Imc)        &
                   ,Tref_land_rnd(Imc),LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc)                 &
                   ,RUNsil_rnd(Imc),CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)                             &
                   ,Lifespan10PAL_success,Lifespan1PAL_success,Lifespan01PAL_success                          &
                   ,Lifespan16p_success,Lifespan18p_success,Lifespan10p_success                               &
                   ,-sumJred_10PAL/Lifespan10PAL_success/1d18                                                 &
                   ,-sumJred_1PAL/Lifespan1PAL_success/1d18                                                   &
                   ,-sumJred_01PAL/Lifespan01PAL_success/1d18                                                 &
                   ,-sumJred_16p/Lifespan16p_success/1d18                                                     &
                   ,-sumJred_18p/Lifespan18p_success/1d18                                                     &
                   ,-sumJred_10p/Lifespan10p_success/1d18                                                     &
                   ,sumJred_10PAL,sumJred_1PAL,sumJred_01PAL,sumJred_16p,sumJred_18p,sumJred_10p
    End If
!
!---------!
! Recycle !
!---------!
    P(1) = P(2)
    N(1) = N(2)
    O(1) = O(2)
    A(1) = A(2)
    G(1) = G(2)
    C(1) = C(2)
    S(1) = S(2)
    PYR(1) = PYR(2)
    GYP(1) = GYP(2)
    CAL(1) = CAL(2)
    ALK(1) = ALK(2)
    delAc(1) = delAc(2)
    delAtmc(1) = delAtmc(2)
    delOc(1) = delOc(2)
    delGc(1) = delGc(2)
    delCc(1) = delCc(2)
    delAs(1) = delAs(2)
    delPYRs(1) = delPYRs(2)
    delGYPs(1) = delGYPs(2)
    d34S(1) = d34S(2)
    d34Spyr(1) = d34Spyr(2)
    d13C(1) = d13C(2)
    d13Corg(1) = d13Corg(2)
    pCO2(1) = pCO2(2)
    pCO2c = pCO2(1)*1d-6*0.987d0
    mO2(1) = mO2(2)
    pO2(1) = pO2(2)
    Mch4(1) = Mch4(2)
    pCH4(1) = pCH4(2)
    m_CH4(1) = m_CH4(2)
    mCO2(1) = mCO2(2)
!
!---------------!
! output        !
!---------------!
    If(Ioutput == 1) then
      Write(*,*) Int(sTime/1d6),' Ma: fO2=',mO2(2)*100d0,' (%)'
      If(sTime >= 0d0) then
      ! atmosphere.dat
        write(15,812) sTime/1d9,pO2(2),pO2(2)/pO20,mO2(2),pCH4(2),m_CH4(2),pCO2(2)/1d6,mCO2(2) &
                     ,pCH4(2)/(pCO2(2)/1d6),RCO2,Patm,f_H2O
      ! reservoir.dat
        write(16,821) sTime/1d9,P(2)/1d15,N(2)/1d15,O(2)/1d18,A(2)/1d18,G(2)/1d18,C(2)/1d18,S(2)/1d18 &
                     ,PYR(2)/1d18,GYP(2)/1d18,CAL(2)/1d18,ALK(2)/1d18,P(2)/Mocean*1d6,N(2)/Mocean*1d6 &
                     ,S(2)/Mocean*1d3,CAL(2)/Mocean*1d3,ALK(2)/Mocean*1d3,(G(2)+C(2))/1d18            &
                     ,(PYR(2)+GYP(2))/1d18,DOA,f_H2O
      ! flux.dat
        write(17,835) sTime/1d9,JsbdCorg/1d12,JsbdSpyr/1d12,JsbdFeox/1d12,JsbdCarb/1d12,JsbdSgyp/1d12 &
                     ,JmanC/1d12,JmanS/1d12,JmanRed/1d12,0.5d0*Jesc_H2O/1d12,0.5d0*Jesc_CH4/1d12,t_ocn &
                     ,Rp/1d12,Wp/1d12,Bporg/1d12,Bpfe/1d12,Bpca/1d12,(Bporg+Bpfe+Bpca)/1d12 &
                     ,Fnfix/1d12,Fdeni/1d12,Bnorg/1d12 &
                     ,Bcorg/1d12,Btcorg/1d12,(Bcorg+Btcorg)/1d12,Worg/1d12,Fmorg/1d12,Wsil/1d12,Wcarb/1d12 &
                     ,Fmcarb/1d12,Bcarb/1d12,Bpyr/1d12,Wpyr/1d12,Bgyp/1d12,Wgyp/1d12,Jch4t/1d12
      ! etc.dat
        write(18,812) sTime/1d9,Tc,Tk,pH,NPPocean*12d0/1d15,NPPland*12d0/1d15       &
                     ,(NPPocean+NPPland)*12d0/1d15,(NPPocean+NPPland)/(NPP0+NPPt0)  &
                     ,fo2,fch4,go2,gch4
      ! GRB.dat
        write(19,803) sTime/1d9,GRBaos/1d12,GRBsurf/1d12
      ! isotope.dat
        write(20,805) sTime/1d9,d13C(2),d13Corg(2),d34S(2),d34Spyr(2)
      ! forcing.dat
        write(21,807) sTime/1d9,fC,fR,fG,fE,fP,fpP
      ! climate.dat
        Write(22,808) sTime/1d9,Tc,Tk,Sun,albedo,TOA,jOLR(pln,Tk,dOLR),radforce_CH4(m_CH4)
      ! vegetation.dat
        Write(23,808) sTime/1d9,V,Vnpp,VnppO2,VnppCO2,VnppT,ignit(mO2),kfire / (kfire-1d0+ignit(mO2))
      ! productivity.dat
        Write(24,810) sTime/1d9,NPPocean*12d0/1d15,NPPland*12d0/1d15,(NPPocean+NPPland)*12d0/1d15 &
                     ,(NPPocean+NPPland)/(NPP0+NPPt0)                                             &
                     ,rCP*N(2)/rNP/Mocean*1d6,rCP*P(2)/Mocean*1d6,(Cco2+Chco3)/rho*1d6            &
                     ,(rCP*P(2)/Mocean*1d6)/(rCP*N(2)/rNP/Mocean*1d6)                             &
                     ,((Cco2+Chco3)/rho*1d6)/(rCP*N(2)/rNP/Mocean*1d6)
      ! carbonate_sys.dat
        Write(25,806) sTime/1d9,pH,Omega,Cco3,Chco3,Cco2
      End If
      Ioutput=0
    End If
!
    End Do
 9998 Continue
    Write(15,*) ' '
    Write(16,*) ' '
    Write(17,*) ' '
    Write(18,*) ' '
    Write(19,*) ' '
    Write(20,*) ' '
    Write(21,*) ' '
    Write(22,*) ' '
    Write(23,*) ' '
    Write(24,*) ' '
    Write(25,*) ' '

    Close(1)
    Close(2)
    Close(3)
    Close(4)
    Close(7)
    Close(8)

    End Do
!
!----------!
!  Format  !
!----------!
    707  Format(7E13.4E2)
    710  Format(10E13.4E2)
    716  Format(16E13.4E2)
    747  Format(47E13.4E2)
    748  Format(48E13.4E2)
    751  Format(51E13.4E2)
    802  Format(2E13.4E2)
    803  Format(3E13.4E2)
    804  Format(4E13.4E2)
    805  Format(5E13.4E2)
    806  Format(6E13.4E2)
    807  Format(7E13.4E2)
    808  Format(8E13.4E2)
    809  Format(9E13.4E2)
    810  Format(10E13.4E2)
    811  Format(11E13.4E2)
    812  Format(12E13.4E2)
    813  Format(13E13.4E2)
    814  Format(14E13.4E2)
    815  Format(15E13.4E2)
    816  Format(16E13.4E2)
    817  Format(17E13.4E2)
    818  Format(18E13.4E2)
    819  Format(19E13.4E2)
    820  Format(20E13.4E2)
    821  Format(21E13.4E2)
    828  Format(28E13.4E2)
    829  Format(29E13.4E2)
    830  Format(30E13.4E2)
    832  Format(32E13.4E2)
    833  Format(33E13.4E2)
    834  Format(34E13.4E2)
    835  Format(35E13.4E2)
    860  Format(60E13.4E2)
    862  Format(62E13.4E2)
    912  Format(I3,11E13.4E2)
    913  Format(I3,12E13.4E2)
    914  Format(I3,13E13.4E2)
    915  Format(I3,14E13.4E2)
    916  Format(I3,15E13.4E2)
    917  Format(I3,16E13.4E2)
    918  Format(I3,17E13.4E2)
    919  Format(I3,18E13.4E2)
    920  Format(I3,19E13.4E2)
!
 9999 Continue
!
!-------------!
!  Functions  !
!-------------!
    CONTAINS
! TOA_low
    FUNCTION TOA_low(albedo,Temp,pCO2bar,dTOA)
      USE Constants
      Implicit none
      double precision:: TOA_low
      double precision, Intent(IN) :: albedo,Temp,pCO2bar,dTOA
      TOA_low = (z1 + z2*albedo + z3*Temp + z4*pCO2bar + z5*mu + z6*albedo*pCO2bar                &
                + z7*mu*pCO2bar + z8*albedo*mu + z9*albedo*Temp + z10*mu*Temp + z11*pCO2bar*Temp  &
                + z12*albedo**2d0 + z13*Temp**2d0 + z14*pCO2bar**2d0 + z15*mu**2d0)*(1d0+dTOA)
    End FUNCTION TOA_low
! TOA_high
    FUNCTION TOA_high(albedo,Temp,pCO2bar,dTOA)
      USE Constants
      Implicit none
      double precision:: TOA_high
      double precision, Intent(IN) :: albedo,Temp,pCO2bar,dTOA
      TOA_high = (zz1 + zz2*albedo + zz3*Temp + zz4*pCO2bar + zz5*mu + zz6*albedo*pCO2bar              &
                + zz7*mu*pCO2bar + zz8*albedo*mu + zz9*albedo*Temp + zz10*mu*Temp + zz11*pCO2bar*Temp  &
                + zz12*albedo**2d0 + zz13*Temp**2d0 + zz14*pCO2bar**2d0 + zz15*mu**2d0)*(1d0+dTOA)
    End FUNCTION TOA_high
! jOLR
    FUNCTION jOLR(pln,Temp,dOLR)
      USE Constants
      Implicit none
      double precision:: jOLR
      double precision, Intent(IN) :: pln,Temp,dOLR
      jOLR = (x1 + x2*pln + x3*Temp + x4*pln*Temp + x5*pln**2d0 + x6*Temp**2d0 + x7*pln**2d0*Temp     &
            + x8*pln*Temp**2d0 + x9*pln**2d0*Temp**2d0 + x10*pln**3d0 + x11*Temp**3d0                 &
            + x12*pln**3d0*Temp + x13*pln**3d0*Temp**2d0 + x14*pln*Temp**3d0 + x15*pln**2d0*Temp**3d0 &
            + x16*pln**3d0*Temp**3d0 + x17*pln**4d0 + x18*pln**4d0*Temp + x19*pln**4d0*Temp**2d0      &
            + x20*pln**4d0*Temp**3d0)*(1d0+dOLR)
    End FUNCTION jOLR
! radforce_CH4
    FUNCTION radforce_CH4(m_CH4)
      USE Constants
      Implicit none
      double precision:: radforce_CH4
      double precision, Intent(IN) :: m_CH4(2)
      ! Radiative forcing of CH4 (Byrne and Goldblatt, 2014)
      If(m_CH4(1) <= 2.5d-6) then
        radforce_CH4 = m1*(dsqrt(m_CH4(1))-dsqrt(715d-9)) - m2*(dsqrt(m_CH4(1))-dsqrt(715d-9))**2d0
      Else
        radforce_CH4 = m3 + m4*dlog(m_CH4(1)/2.5d-6) + m5*(dlog(m_CH4(1)/2.5d-6))**2d0
      End If
    End FUNCTION radforce_CH4
! albedo
    FUNCTION albedo_T(Temp)
      USE Constants
      Implicit none
      double precision:: albedo_T
      double precision, Intent(IN) :: Temp
      albedo_T = aice
      If(Temp > Tice) then
        albedo_T = aice - (aice-a00)*(Temp-Tice)**2d0/(T0-Tice)**2d0
      End If
      If(Temp >= T0) then
        albedo_T = a00
      End If
    End FUNCTION albedo_T
! lnK0
    FUNCTION lnK0(Temp)
      USE Constants
      Implicit none
      double precision:: lnK0
      double precision, Intent(IN) :: Temp
      lnK0 = -60.2409d0 + 93.4517d0*(100d0/Temp) + 23.3585d0*dlog(Temp/100d0)  &
             + Sal*(0.023517d0-0.023656d0*Temp/100d0+0.0047036d0*(Temp/100d0)**2d0)
    End FUNCTION lnK0
! logK1
    FUNCTION logK1(Temp)
      USE Constants
      Implicit None
      double precision:: logK1
      double precision, Intent(IN) :: Temp
      logK1 = 6.2008d1 - 3.6707d3/Temp - 9.7944d0*dlog(Temp) + 0.0118d0*Sal - 1.16d-4*Sal*Sal
    End FUNCTION logK1
! logK2
    FUNCTION logK2(Temp)
      USE Constants
      Implicit none
      double precision:: logK2
      double precision, Intent(IN) :: Temp
      logK2 = -4.777d0 - 1.3947d3/Temp + 1.84d-2*Sal - 1.18d-4*Sal*Sal
    End FUNCTION logK2
! lnKspCal
    FUNCTION lnKspCal(Temp)
      USE Constants
      double precision:: lnKspCal
      double precision, Intent(IN) :: Temp
      lnKspCal = -1.719065d2 - 7.7993d-2*Temp + 2.839319d3/Temp  &
                 + 7.1595d1*dlog10(Temp) + (-7.7712d-1+2.8426d-3*Temp+178.34d0/Temp)*dsqrt(Sal)  &
                 - 7.711d-2*Sal + 4.1249d-3*Sal*dsqrt(Sal)
    End FUNCTION lnKspCal
! ignition factor
    FUNCTION ignit(mO2)
      USE Constants
      double precision:: ignit
      double precision, Intent(IN) :: mO2(2)
      ! ignit = 586.2d0*mO2(1) - 122.102d0   ! Ignition factor (Bergman et al., 2004)
      ignit = 48d0*mO2(1)-9.08d0             ! Ignition factor (Lenton, 2013)
      If(ignit < 0d0) then
        ignit = 0d0
      End If
      If(ignit > 5d0) then
        ignit = 5d0
      End If
    End FUNCTION ignit
! fTmarine
    FUNCTION fTmarine(Tc,Tref)
      USE Constants
      double precision:: fTmarine
      double precision, Intent(IN) :: Tc,Tref
      ! Temperature effect on marine biosphere
        If(Tc <= 28d0) then
           fTmarine = 1d0 - ((Topt-Tc)/dTopt)**2d0
        Else
          fTmarine = (1d0-((Topt-Tc)/dTopt)**2d0) * dexp(-dabs(Tc-Topt)**3d0/Tref)
        End If
        If(fTmarine < 0d0) then
          fTmarine = 0d0
        End If
    End FUNCTION fTmarine
!
!
    End Program LIFESPAN
!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
  Subroutine RANDOM(nG_rnd,mG_rnd,aG_rnd,tauG_rnd,aR_rnd,tauR_rnd,JsbdCorg0_rnd                                 &
                   ,JsbdSpyr0_rnd,JsbdFeox0_rnd,JsbdCarb0_rnd,JsbdSgyp0_rnd,JmanC0_rnd,JmanS0_rnd,JmanRed0_rnd  &
                   ,Corg0_rnd,Spyr0_rnd,Carb0_rnd,Sgyp0_rnd,fUV_rnd,Tref_rnd,Tref_land_rnd,LIFE_rnd             &
                   ,FERT_rnd,ACT_rnd,Pmin_rnd,RUNsil_rnd,CPanoxic_rnd,dOLR_rnd,dTOA_rnd)
    USE Constants
    Implicit none
    Integer:: I,seedsize,clock
    Integer, allocatable :: seed(:)
    double precision:: rnd1,rnd2,mean,sigma,Xrnd1,Xrnd2
    double precision:: nG_rnd(resample),mG_rnd(resample)
    double precision:: aG_rnd(resample),tauG_rnd(resample),aR_rnd(resample),tauR_rnd(resample)
    double precision:: JsbdCorg0_rnd(resample),JsbdSpyr0_rnd(resample),JsbdFeox0_rnd(resample)
    double precision:: JsbdCarb0_rnd(resample),JsbdSgyp0_rnd(resample)
    double precision:: JmanC0_rnd(resample),JmanS0_rnd(resample),JmanRed0_rnd(resample)
    double precision:: Corg0_rnd(resample),Carb0_rnd(resample),Spyr0_rnd(resample),Sgyp0_rnd(resample)
    double precision:: fUV_rnd(resample),Tref_rnd(resample),Tref_land_rnd(resample)
    double precision:: LIFE_rnd(resample),FERT_rnd(resample),ACT_rnd(resample)
    double precision:: Pmin_rnd(resample),RUNsil_rnd(resample),CPanoxic_rnd(resample)
    double precision:: dOLR_rnd(resample),dTOA_rnd(resample)
!
! Random number !
    Call random_seed(size=seedsize)
    allocate(seed(seedsize))
    Call system_clock(count=clock)
    seed = clock
    Call random_seed(put=seed)
!
!-----------------------!
    Do I = 1, resample  !
!-----------------------!
    ! nG: 0-0.73
      Call random_number(rnd1)
      nG_rnd(I) = 0.73d0*rnd1
      !nG_rnd(I) = 0.73d0
    ! mG: 1-2
      Call random_number(rnd1)
      mG_rnd(I) = 1d0 + 1d0*rnd1
      !mG_rnd(I) = 1d0
    ! aG: 0-0.5
      Call random_number(rnd1)
      aG_rnd(I) = 0.5d0*rnd1
      !aG_rnd(I) = 0.25d0
    ! tG: 100-500 Myr
      Call random_number(rnd1)
      tauG_rnd(I) = 100d0 + 400d0*rnd1
      !tauG_rnd(I) = 300d0
    ! aR: 0-0.5
      Call random_number(rnd1)
      aR_rnd(I) = 0.5d0*rnd1
      !aR_rnd(I) = 0.25d0
    ! tR: 100-500 Myr
      Call random_number(rnd1)
      tauR_rnd(I) = 100d0 + 400d0*rnd1
      !tauR_rnd(I) = 300d0
    ! JsbdCorg0: 0-0.5 TmolC/yr
      Call random_number(rnd1)
      JsbdCorg0_rnd(I) = 1d12*rnd1
      !JsbdCorg0_rnd(I) = 0.05d12
    ! JsbdSpyr0: 0-1 TmolS/yr
      Call random_number(rnd1)
      JsbdSpyr0_rnd(I) = 1d12*rnd1
      !JsbdSpyr0_rnd(I) = 0.2d12
    ! JsbdFeox0: 0-7 TmolFe/yr
      Call random_number(rnd1)
      JsbdFeox0_rnd(I) = 7d12*rnd1
      !JsbdFeox0_rnd(I) = 1d12
    ! JsbdCarb0: 0-1.5 TmolC/yr
      Call random_number(rnd1)
      JsbdCarb0_rnd(I) = 1.5d12*rnd1
      !JsbdCarb0_rnd(I) = 0.5d12
    ! JsbdSgyp0: 0 TmolS/yr
      Call random_number(rnd1)
      !JsbdSgyp0_rnd(I) = 0.5d12*rnd1
      JsbdSgyp0_rnd(I) = 0d12*rnd1
      !JsbdSgyp0_rnd(I) = 0d12
    ! JmanC0: 0-3 TmolC/yr
      Call random_number(rnd1)
      JmanC0_rnd(I) = 3d12*rnd1
      !JmanC0_rnd(I) = 1d12
    ! JmanS0: 0-1 TmolS/yr
      Call random_number(rnd1)
      JmanS0_rnd(I) = 1d12*rnd1
      !JmanS0_rnd(I) = 1d12
    ! JmanRed0: 0-1 Tmol/yr
      Call random_number(rnd1)
      JmanRed0_rnd(I) = 1d12*rnd1
      !JmanRed0_rnd(I) = 0d12
    ! Corg0: 750-1750 EmolC
      Call random_number(rnd1)
      Corg0_rnd(I) = (750d0 + 1000d0*rnd1)*1d18
      !Corg0_rnd(I) = 1250d18
    ! Spyr0: 100-300 EmolS
      Call random_number(rnd1)
      Spyr0_rnd(I) = (100d0 + 200d0*rnd1)*1d18
      !Spyr0_rnd(I) = 200d18
    ! Carb0: 4000-6000 EmolC
      Call random_number(rnd1)
      Carb0_rnd(I) = (4000d0 + 2000d0*rnd1)*1d18
      !Carb0_rnd(I) = 5000d18
    ! Sgyp0: 100-300 EmolS
      Call random_number(rnd1)
      Sgyp0_rnd(I) = (100d0 + 200d0*rnd1)*1d18
      !Sgyp0_rnd(I) = 200d18
    ! fUV: 10^-2.5 - 10^-1.5
      Call random_number(rnd1)
      fUV_rnd(I) = 10d0**(-1.5d0 - 1d0*rnd1)
      !fUV_rnd(I) = 10d0**(-1d0)
    ! Tref: 1400-7000K
      Call random_number(rnd1)
      Tref_rnd(I) = 1400d0 + 5600d0*rnd1
    ! Tref_land: 25-35C
      Call random_number(rnd1)
      Tref_land_rnd(I) = 25d0 + 10d0*rnd1
    ! LIFE: 0.1-0.4
      Call random_number(rnd1)
      LIFE_rnd(I) = 0.1d0 + 0.3d0*rnd1
    ! FERT: 0.2-0.6
      Call random_number(rnd1)
      FERT_rnd(I) = 0.2d0 + 0.4d0*rnd1
    ! ACT: 0.045-0.135
      Call random_number(rnd1)
      ACT_rnd(I) = 0.045d0 + 0.09d0*rnd1
    ! Pmin: 1-10 ppmv
      Call random_number(rnd1)
       Pmin_rnd(I) = 1d0 + 9d0*rnd1
    ! RUNsil: 0.025-0.045
      Call random_number(rnd1)
      RUNsil_rnd(I) = 0.025d0 + 0.02d0*rnd1
    ! CPanoxic: 1x-20xCPoxic
      Call random_number(rnd1)
      CPanoxic_rnd(I) = CPoxic * (1d0 + 19d0*rnd1)
!-----------------------!
    End Do              !
!-----------------------!
    ! OLR: Outgoing-Longwave-Radiation
    Do I = 1, resample/2
      Call random_number(rnd1)
      Call random_number(rnd2)
      mean  = 0d0
      sigma = 0.02d0**2d0
      Xrnd1 = dsqrt(sigma)*dsqrt(-2d0*dlog(rnd1))*dcos(2d0*pi*rnd2) + mean
      Xrnd2 = dsqrt(sigma)*dsqrt(-2d0*dlog(rnd1))*dsin(2d0*pi*rnd2) + mean
      dOLR_rnd(2*I-1) = Xrnd1
      dOLR_rnd(2*I)   = Xrnd2
!      dOLR_rnd(2*I-1) = 0d0
!      dOLR_rnd(2*I)   = 0d0
    End Do
    ! TOA: Top-of-Atmosphere 
    Do I = 1, resample/2
      Call random_number(rnd1)
      Call random_number(rnd2)
      mean  = 0d0
      sigma = 0.02d0**2d0
      Xrnd1 = dsqrt(sigma)*dsqrt(-2d0*dlog(rnd1))*dcos(2d0*pi*rnd2) + mean
      Xrnd2 = dsqrt(sigma)*dsqrt(-2d0*dlog(rnd1))*dsin(2d0*pi*rnd2) + mean
      dTOA_rnd(2*I-1) = Xrnd1
      dTOA_rnd(2*I)   = Xrnd2
!      dTOA_rnd(2*I-1) = 0d0
!      dTOA_rnd(2*I)   = 0d0
    End Do

    Return
  End Subroutine RANDOM
!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
  Subroutine SETUP(Imc,JsbdCorg0,JsbdSpyr0,JsbdCarb0,JsbdSgyp0,JmanC0,JmanS0,JmanRed0 &
                  ,JsbdCorg0_rnd,JsbdSpyr0_rnd,JsbdCarb0_rnd,JsbdSgyp0_rnd            &
                  ,JmanC0_rnd,JmanS0_rnd,JmanRed0_rnd                                 &
                  ,nG_rnd,mG_rnd,aG_rnd,tauG_rnd,aR_rnd,tauR_rnd                      &
                  ,Corg0_rnd,Spyr0_rnd,Carb0_rnd,Sgyp0_rnd                            &
                  ,fUV_rnd,Tref_rnd,Tref_land_rnd,LIFE_rnd,FERT_rnd                   &
                  ,ACT_rnd,Pmin_rnd,RUNsil_rnd,CPanoxic_rnd,dOLR_rnd,dTOA_rnd         &
                  ,Fmcarb0,Worg0,Bgyp0,Bpyr0                                          &
                  ,aG,aR,tauG,tauR,nG,mG,fUV,Tref,Tref_land,LIFE,FERT,ACT             &
                  ,Pmin,Kp,RUNsil,CPanoxic,dOLR,dTOA)
    USE Constants
    Implicit none
    Integer:: Imc
    double precision:: JsbdCorg0,JsbdSpyr0,JsbdCarb0,JsbdSgyp0,JmanC0,JmanS0,JmanRed0
    double precision:: JsbdCorg0_rnd(resample),JsbdSpyr0_rnd(resample)
    double precision:: JsbdCarb0_rnd(resample),JsbdSgyp0_rnd(resample)
    double precision:: JmanC0_rnd(resample),JmanS0_rnd(resample),JmanRed0_rnd(resample)
    double precision:: nG_rnd(resample),mG_rnd(resample),aG_rnd(resample),tauG_rnd(resample)
    double precision:: aR_rnd(resample),tauR_rnd(resample)
    double precision:: Corg0_rnd(resample),Spyr0_rnd(resample),Carb0_rnd(resample),Sgyp0_rnd(resample)
    double precision:: fUV_rnd(resample),Tref_rnd(resample),Tref_land_rnd(resample)
    double precision:: LIFE_rnd(resample),FERT_rnd(resample)
    double precision:: ACT_rnd(resample),Pmin_rnd(resample),RUNsil_rnd(resample)
    double precision:: CPanoxic_rnd(resample),dOLR_rnd(resample),dTOA_rnd(resample)
    double precision:: Fmcarb0,Worg0,Bgyp0,Bpyr0
    double precision:: aG,aR,tauG,tauR,nG,mG,fUV,Tref,Tref_land,LIFE,FERT,ACT
    double precision:: Pmin,Kp,RUNsil,CPanoxic,dOLR,dTOA
!
  ! all.dat
    Write(10,128) nG_rnd(Imc),mG_rnd(Imc),aG_rnd(Imc),tauG_rnd(Imc),aR_rnd(Imc),tauR_rnd(Imc)    &
                 ,JsbdCorg0_rnd(Imc),JsbdSpyr0_rnd(Imc),JsbdCarb0_rnd(Imc),JsbdSgyp0_rnd(Imc)    &
                 ,JmanC0_rnd(Imc),JmanS0_rnd(Imc),JmanRed0_rnd(Imc)                              &
                 ,Corg0_rnd(Imc),Spyr0_rnd(Imc),Carb0_rnd(Imc),Sgyp0_rnd(Imc)                    &
                 ,fUV_rnd(Imc),Tref_rnd(Imc),Tref_land_rnd(Imc)                                  &
                 ,LIFE_rnd(Imc),FERT_rnd(Imc),ACT_rnd(Imc),Pmin_rnd(Imc),RUNsil_rnd(Imc)         &
                 ,CPanoxic_rnd(Imc),dOLR_rnd(Imc),dTOA_rnd(Imc)
  ! Subduction flux
    JsbdCorg0 = JsbdCorg0_rnd(Imc)
    JsbdSpyr0 = JsbdSpyr0_rnd(Imc)
    JsbdCarb0 = JsbdCarb0_rnd(Imc)
    JsbdSgyp0 = JsbdSgyp0_rnd(Imc)
  ! Mantle degassing flux
    JmanC0    = JmanC0_rnd(Imc)
    JmanS0    = JmanS0_rnd(Imc)
    JmanRed0  = JmanRed0_rnd(Imc)
  ! Other fluxes determined based on mass-balance
    Fmcarb0 = Bcarb0 - Wcarb0 - JsbdCarb0           ! CO2 degassing via carbonate metamorphism
    Worg0   = BtCorg0 + Bcorg0 - Fmorg0 - JsbdCorg0 ! Oxidative weathering of organic C
    Bgyp0   = Wgyp0 + JsbdSgyp0                     ! Gypsum burial
    Bpyr0   = Wpyr0 + JsbdSpyr0                     ! Pyrite burial
  ! Other uncertain parameters
    aG        = aG_rnd(Imc)
    aR        = aR_rnd(Imc)
    tauG      = tauG_rnd(Imc)
    tauR      = tauR_rnd(Imc)
    nG        = nG_rnd(Imc)
    mG        = mG_rnd(Imc)
    fUV       = fUV_rnd(Imc)
    Tref      = Tref_rnd(Imc)
    Tref_land = Tref_land_rnd(Imc)
    LIFE      = LIFE_rnd(Imc)
    FERT      = FERT_rnd(Imc)
    ACT       = ACT_rnd(Imc)
    Pmin      = Pmin_rnd(Imc)
    Kp        = (2d0*(1d0-((25d0-15d0)/25d0)**2d0))*(pCO20-Pmin)
    RUNsil    = RUNsil_rnd(Imc)
    CPanoxic  = CPanoxic_rnd(Imc)
    dOLR      = dOLR_rnd(Imc)
    dTOA      = dTOA_rnd(Imc)
!
128 Format(28E13.4E2)
!
    Return
  End Subroutine SETUP
!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
  Subroutine INIT(Imc,CPland,CNorg,Tk,Tc,DelP,Bcarb,f_H2O,Jesc_H2O,P,N,O,A,G,C,S,PYR,GYP,CAL,ALK   &
                 ,Corg0_rnd,Carb0_rnd,Spyr0_rnd,Sgyp0_rnd,P_success,N_success,O_success,A_success  &
                 ,G_success,C_success,S_success,PYR_success,GYP_success,CAL_success,ALK_success    &
                 ,JsbdCorg_success,JsbdCorg0_rnd,JsbdSpyr_success,JsbdSpyr0_rnd,JsbdCarb_success   &
                 ,JsbdCarb0_rnd,JsbdSgyp_success,JsbdSgyp0_rnd,JmanC_success,JmanC0,JmanS_success  &
                 ,JmanS0,JmanRed_success,JmanRed0,GRBaos_success,GRBsurf_success                   &
                 ,pO2_success,mO2_success,pCH4_success,pCO2_success,m_CH4_success,mCO2_success     &
                 ,Tc_success,Tk_success,pH_success,NPPocean_success,NPPland_success,f_H2O_success  &
                 ,Jesc_H2O_success,Jesc_CH4_success,t_ocn_success,RCO2_success                     &
                 ,d34S_success,d34Spyr_success,d13C_success,d13Corg_success                        &
                 ,DOA_success,Patm_success,Sun_success,albedo_success,V_success,Vnpp_success       &
                 ,VnppO2_success,VnppCO2_success,VnppT_success,ignit_success,fire_success          &
                 ,Omega_success,Cco3_success,Chco3_success,Cco2_success,Rp_success,Wp_success      &
                 ,Bporg_success,Bpfe_success,Bpca_success,Fnfix_success,Fdeni_success              &
                 ,Bnorg_success,Bcorg_success,Btcorg_success,Worg_success,Fmorg_success            &
                 ,Wsil_success,Wcarb_success,Fmcarb_success,Bcarb_success,Bpyr_success             &
                 ,Wpyr_success,Bgyp_success,Wgyp_success                                           &
                 ,fo2_success,fch4_success,go2_success,gch4_success,Jch4t_success                  &
                 ,delCc,delGc,delAtmc,delAc,delOc,delAs,delGYPs,d34S,delPYRs,d34Spyr,d13C,d13Corg  &
                 ,pCO2,RCO2,pCO2c,mO2,pO2,MCH40,Mch4,Mo2_ox,Mch4_ox,phi_o2,phi_ch4,pCH4,pCH4c      &
                 ,Jesc_CH4,psiO2,PSIch4,gamma,delta,fco2limit,Patm,Matm_mol                        &
                 ,thuman,thuman2,thuman3,tanimal,toae,t1pPAL,tozone,Ihuman,Ihuman2,Ihuman3,Ianimal &
                 ,Ioae,I1pPAL,Iozone,judge,Ichuman,Ichuman2,Ichuman3,Ic1pPAL,Ic10pPAL,Icount3      &
                 ,Isucc,Isuc2,sumJred,sumTime,sumGRBexo,sumJman,sumJsbd,Time,sTime,Ij,Ij2,Ioutput  &
                 ,kch4ox,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,dt,Fmcarb0,Worg0,Bgyp0,Bpyr0)
    USE Constants
    Implicit none
    Integer:: Imc,Ihuman,Ihuman2,Ihuman3,Ianimal,Ioae,I1pPAL,Iozone,judge,Ichuman,Ichuman2,Ichuman3
    Integer:: Ic1pPAL,Ic10pPAL,Icount3,Isucc,Isuc2,Ij,Ij2,Ioutput
    double precision:: CPland,CNorg,Tk,Tc,DelP,Bcarb,f_H2O,Jesc_H2O
    double precision:: P(2),N(2),O(2),A(2),G(2),C(2),S(2),PYR(2),GYP(2),CAL(2),ALK(2)
    double precision:: Corg0_rnd(resample),Carb0_rnd(resample),Spyr0_rnd(resample),Sgyp0_rnd(resample)
    double precision:: P_success(600),N_success(600),O_success(600),A_success(600),G_success(600),C_success(600)
    double precision:: S_success(600),PYR_success(600),GYP_success(600),CAL_success(600),ALK_success(600)
    double precision:: JsbdCorg_success(600),JsbdCorg0_rnd(resample),JsbdSpyr_success(600),JsbdSpyr0_rnd(resample)
    double precision:: JsbdCarb_success(600),JsbdCarb0_rnd(resample),JsbdSgyp_success(600),JsbdSgyp0_rnd(resample)
    double precision:: JmanC_success(600),JmanC0,JmanS_success(600),JmanS0,JmanRed_success(600),JmanRed0
    double precision:: GRBaos_success(600),GRBsurf_success(600)
    double precision:: pO2_success(600),mO2_success(600),pCH4_success(600),pCO2_success(600),m_CH4_success(600)
    double precision:: mCO2_success(600),Tc_success(600),Tk_success(600),pH_success(600),NPPocean_success(600)
    double precision:: NPPland_success(600),f_H2O_success(600),Jesc_H2O_success(600),Jesc_CH4_success(600)
    double precision:: t_ocn_success(600),RCO2_success(600)
    double precision:: d34S_success(600),d34Spyr_success(600),d13C_success(600),d13Corg_success(600)
    double precision:: DOA_success(600),Patm_success(600),Sun_success(600),albedo_success(600)
    double precision:: V_success(600),Vnpp_success(600),VnppO2_success(600),VnppCO2_success(600),VnppT_success(600)
    double precision:: ignit_success(600),fire_success(600),Omega_success(600)
    double precision:: Cco3_success(600),Chco3_success(600),Cco2_success(600)
    double precision:: Rp_success(600),Wp_success(600),Bporg_success(600),Bpfe_success(600),Bpca_success(600)
    double precision:: Fnfix_success(600),Fdeni_success(600),Bnorg_success(600),Bcorg_success(600),Btcorg_success(600)
    double precision:: Worg_success(600),Fmorg_success(600),Wsil_success(600),Wcarb_success(600),Fmcarb_success(600)
    double precision:: Bcarb_success(600),Bpyr_success(600),Wpyr_success(600),Bgyp_success(600),Wgyp_success(600)
    double precision:: fo2_success(600),fch4_success(600),go2_success(600),gch4_success(600),Jch4t_success(600)
    double precision:: delCc(2),delGc(2),delAtmc(2),delAc(2),delOc(2),delAs(2)
    double precision:: delGYPs(2),d34S(2),delPYRs(2),d34Spyr(2),d13C(2),d13Corg(2)
    double precision:: pCO2(2),RCO2,pCO2c,mO2(2),pO2(2),MCH40,Mch4(2),Mo2_ox,Mch4_ox,phi_o2,phi_ch4,pCH4(2),pCH4c
    double precision:: Jesc_CH4,psiO2,PSIch4,gamma,delta,fco2limit,Patm,Matm_mol
    double precision:: thuman,thuman2,thuman3,tanimal,toae,t1pPAL,tozone
    double precision:: sumJred,sumTime,sumGRBexo,sumJman,sumJsbd,Time,sTime
    double precision:: kch4ox,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,dt
    double precision:: Fmcarb0,Worg0,Bgyp0,Bpyr0

! Time step [yr]
    dt = 1d0
!
    CPland = CPland0        ! Initial value of C/P of terrestrial biomass
    CNorg = CNorg0          ! Initial value of N/P
    Tk = Tk0                ! Initial value of surface temp. [K]
    Tc = Tc0                ! Initial value of surface temp. [C]
    DelP = DelP0            ! Initial value of isotopic fractionation [o/oo]
    Bcarb = Bcarb0          ! Initial value of carbonate burial [mol/yr]
    f_H2O = 10d0**(fesc0 + fesc1*Tk + fesc2*Tk*Tk + fesc3*Tk**3d0) ! Mixing ratio of H2O in the stratosphere
    Jesc_H2O = fesc * f_H2O ! [molH2/yr]
    P(1) = P0               ! Phosphate
    N(1) = N0               ! Nitrate             
    O(1) = O0               ! Oxygen
    A(1) = A0*5d0           ! Inorganic C
    G(1) = Corg0_rnd(Imc)   ! Crustal organic C
    C(1) = Carb0_rnd(Imc)   ! Crustal carbonate C
    S(1) = S0               ! Sulfate
    PYR(1) = Spyr0_rnd(Imc) ! Crustal pyrite S
    GYP(1) = Sgyp0_rnd(Imc) ! Crustal gypsum S
    CAL(1) = CAL0           ! Calcium
    ALK(1) = ALK0*4d0       ! Alkalinity
!
    P_success(1) = P0
    N_success(1) = N0
    O_success(1) = O0
    A_success(1) = A0*5d0
    G_success(1) = Corg0_rnd(Imc)
    C_success(1) = Carb0_rnd(Imc)
    S_success(1) = S0
    PYR_success(1) = Spyr0_rnd(Imc)
    GYP_success(1) = Sgyp0_rnd(Imc)
    CAL_success(1) = CAL0
    ALK_success(1) = ALK0*5d0
    JsbdCorg_success(1) = JsbdCorg0_rnd(Imc)
    JsbdSpyr_success(1) = JsbdSpyr0_rnd(Imc)
    JsbdCarb_success(1) = JsbdCarb0_rnd(Imc)
    JsbdSgyp_success(1) = JsbdSgyp0_rnd(Imc)
    JmanC_success(1) = JmanC0
    JmanS_success(1) = JmanS0
    JmanRed_success(1) = JmanRed0
    GRBaos_success(1) = 0d0
    GRBsurf_success(1) = 0d0
!
    delCc(1) = 0.4d0
    delGc(1) = delCc(1) - DelC0
    delAtmc(1) = DelP0 + DelC0 - delCc(1) + 2d0*delGc(1)
    delAc(1) = delAtmc(1) + (1d0-phi)*(9483d0/Tk-23.89d0)
    delOc(1) = delAtmc(1) + phi*(9483d0/Tk-23.89d0)
    delAs(1) = 20d0
    delGYPs(1) = 20d0
    d34S(1) = delGYPs(1)
    delPYRs(1) = d34S(1) - DelS0
    d34Spyr(1) = delPYRs(1)
    d13C(1) = delOc(1) + 15.10d0 - 4232d0/Tk + 0.9d0
    d13Corg(1) = d13C(1) - DelC0
    pCO2(1) = pCO20*50d0
    RCO2 = pCO2(1)/pCO20
    RCO2_success(1) = RCO2
    pCO2c = pCO2(1)*1d-6*0.987d0
    mO2(1) = 0.21d0
    pO2(1) = 0.21d0
    Mch40 = (143.818d0+38d0)*1d18*pCH40
    Mch4(1) = Mch40
    Mo2_ox = O(1)
    Mch4_ox = Mch4(1)
    pCH4(1) = pCH40*Mch4(1)/Mch40
    pCH4c   = pCH4(1)
    phi_o2  = dlog10(pO2(1)/0.987d0)
    phi_ch4 = dlog10(pCH4(1)/0.987d0)
    If(phi_o2 <= -9d0) then
        kch4ox_6  = c0l6 + c1l6*phi_o2 + c2l6*phi_o2*phi_o2 + c3l6*phi_o2**3d0 &
                         + c4l6*phi_o2**4d0 + c5l6*phi_o2**5d0 + c6l6*phi_o2**6d0
        kch4ox_5  = c0l5 + c1l5*phi_o2 + c2l5*phi_o2*phi_o2 + c3l5*phi_o2**3d0 &
                         + c4l5*phi_o2**4d0 + c5l5*phi_o2**5d0 + c6l5*phi_o2**6d0
        kch4ox_4  = c0l4 + c1l4*phi_o2 + c2l4*phi_o2*phi_o2 + c3l4*phi_o2**3d0 &
                         + c4l4*phi_o2**4d0 + c5l4*phi_o2**5d0 + c6l4*phi_o2**6d0
        kch4ox_3  = c0l3 + c1l3*phi_o2 + c2l3*phi_o2*phi_o2 + c3l3*phi_o2**3d0 &
                         + c4l3*phi_o2**4d0 + c5l3*phi_o2**5d0 + c6l3*phi_o2**6d0
        kch4ox_23 = c0l23 + c1l23*phi_o2 + c2l23*phi_o2*phi_o2 + c3l23*phi_o2**3d0 &
                          + c4l23*phi_o2**4d0 + c5l23*phi_o2**5d0 + c6l23*phi_o2**6d0
    Else If((phi_o2 > -9d0).and.(phi_o2 <= -3d0)) then
        kch4ox_6  = c0m6 + c1m6*phi_o2 + c2m6*phi_o2*phi_o2 + c3m6*phi_o2**3d0 &
                         + c4m6*phi_o2**4d0 + c5m6*phi_o2**5d0 + c6m6*phi_o2**6d0
        kch4ox_5  = c0m5 + c1m5*phi_o2 + c2m5*phi_o2*phi_o2 + c3m5*phi_o2**3d0 &
                         + c4m5*phi_o2**4d0 + c5m5*phi_o2**5d0 + c6m5*phi_o2**6d0
        kch4ox_4  = c0m4 + c1m4*phi_o2 + c2m4*phi_o2*phi_o2 + c3m4*phi_o2**3d0 &
                         + c4m4*phi_o2**4d0 + c5m4*phi_o2**5d0 + c6m4*phi_o2**6d0
        kch4ox_3  = c0m3 + c1m3*phi_o2 + c2m3*phi_o2*phi_o2 + c3m3*phi_o2**3d0 &
                         + c4m3*phi_o2**4d0 + c5m3*phi_o2**5d0 + c6m3*phi_o2**6d0
        kch4ox_23 = c0m23 + c1m23*phi_o2 + c2m23*phi_o2*phi_o2 + c3m23*phi_o2**3d0 &
                         + c4m23*phi_o2**4d0 + c5m23*phi_o2**5d0 + c6m23*phi_o2**6d0
    Else
        kch4ox_6  = c0h6 + c1h6*phi_o2 + c2h6*phi_o2*phi_o2 + c3h6*phi_o2**3d0
        kch4ox_5  = c0h5 + c1h5*phi_o2 + c2h5*phi_o2*phi_o2 + c3h5*phi_o2**3d0
        kch4ox_4  = c0h4 + c1h4*phi_o2 + c2h4*phi_o2*phi_o2 + c3h4*phi_o2**3d0
        kch4ox_3  = c0h3 + c1h3*phi_o2 + c2h3*phi_o2*phi_o2 + c3h3*phi_o2**3d0
        kch4ox_23 = c0h23 + c1h23*phi_o2 + c2h23*phi_o2*phi_o2 + c3h23*phi_o2**3d0
    End If
    If(phi_ch4 <= -6d0) then
        kch4ox = kch4ox_6
    Else If((phi_ch4 > -6d0).and.(phi_ch4 <= -5d0)) then
        kch4ox = kch4ox_6 + (kch4ox_5 - kch4ox_6) * (phi_ch4 + 6d0)
    Else If((phi_ch4 > -5d0).and.(phi_ch4 <= -4d0)) then
        kch4ox = kch4ox_5 + (kch4ox_4 - kch4ox_5) * (phi_ch4 + 5d0)
    Else If((phi_ch4 > -4d0).and.(phi_ch4 <= -3d0)) then
        kch4ox = kch4ox_4 + (kch4ox_3 - kch4ox_4) * (phi_ch4 + 4d0)
    Else If((phi_ch4 > -3d0).and.(phi_ch4 <= dlog10(2d-3))) then
        kch4ox = kch4ox_3 + (kch4ox_23 - kch4ox_3) * (phi_ch4 + 3d0) / (dlog10(2d-3)+3d0)
    Else
        kch4ox = kch4ox_23
    End If
    kch4ox = 10d0**kch4ox
    Jesc_CH4 = 2d0*fesc*pCH4(1) ! [molH2/yr]
    psiO2  = dlog10(pO2(1)*(143.818d0+38d0)*1d18)
    PSIch4 = 10d0**(a1ch4*psiO2**4d0+a2ch4*psiO2**3d0+a3ch4*psiO2**2d0+a4ch4*psiO2+a5ch4)
    gamma = O(1) / (O(1)+dr)
    delta = O(1) / (O(1)+dd)
    Tk = 300d0
    fco2limit = 1d0
!
    pO2_success(1) = pO2(1)
    mO2_success(1) = mO2(1)
    pCH4_success(1) = pCH4(1)
    pCO2_success(1) = pCO2(1)
    m_CH4_success(1) = pCH4(1)
    mCO2_success(1) = pCO2(1)
    d34S_success(1) = d34S(1)
    d34Spyr_success(1) = d34Spyr(1)
    d13C_success(1) = d13C(1)
    d13Corg_success(1) = d13Corg(1)
!
    Tc_success(1) = 20d0
    Tk_success(1) = 293.15d0
    pH_success(1) = 8d0
    NPPocean_success(1) = 3750d0
    NPPland_success(1) = 5000d0
    f_H2O_success(1) = f_H2O
    Jesc_H2O_success(1) = Jesc_H2O
    Jesc_CH4_success(1) = 2d0*fesc*pCH4(1)
    t_ocn_success(1) = MocnH2O/Jesc_H2O
!
    Patm = 1d0
    Matm_mol = Matm_N20 + O(1) + Matm_Ar0 + pCO2(1)/1d6*Matm_mol0 + Mch4(1)
    write(*,*) 'Matm_CO20 =',Matm_CO20,' mol'
!
    DOA_success(1) = DOA0
    Patm_success(1) = 1d0
    Sun_success(1) = Sun0
    albedo_success(1) = a00
    V_success(1) = 0d0
    Vnpp_success(1) = 0d0
    VnppO2_success(1) = 0d0
    VnppCO2_success(1) = 0d0
    VnppT_success(1) = 0d0
    ignit_success(1) = 0d0
    fire_success(1) = 0d0
    Omega_success(1) = 5d0
    Cco3_success(1) = 0d0
    Chco3_success(1) = 0d0
    Cco2_success(1) = 0d0
    Rp_success(1) = Wp0 - Pland0
    Wp_success(1) = Wp0
    Bporg_success(1) = Bcorg0/CPorg0
    Bpfe_success(1) = Bpfe0
    Bpca_success(1) = Bpca0
    Fnfix_success(1) = Fnfix0
    Fdeni_success(1) = Fdeni0
    Bnorg_success(1) = Bcorg0/CNorg0
    Bcorg_success(1) = Bcorg0
    Btcorg_success(1) = Btcorg0
    Worg_success(1) = Worg0
    Fmorg_success(1) = Fmorg0
    Wsil_success(1) = Wsil0
    Wcarb_success(1) = Wcarb0
    Fmcarb_success(1) = Fmcarb0
    Bcarb_success(1) = Bcarb0
    Bpyr_success(1) = Bpyr0
    Wpyr_success(1) = Wpyr0
    Bgyp_success(1) = Bgyp0
    Wgyp_success(1) = Wgyp0
    fo2_success(1) = 1d0
    fch4_success(1) = 0d0
    go2_success(1) = 1d0
    gch4_success(1) = 0d0
    Jch4t_success(1) = 0d12
!
    thuman    = 9999d0
    thuman2   = 9999d0
    thuman3   = 9999d0
    tanimal   = 9999d0
    toae      = 9999d0
    t1pPAL    = 9999d0
    tozone    = 9999d0
    Ihuman    = 0
    Ihuman2   = 0
    Ihuman3   = 0
    Ianimal   = 0
    Ioae      = 0
    I1pPAL    = 0
    Iozone    = 0
    judge     = 999
    Ichuman   = 0
    Ichuman2  = 0
    Ichuman3  = 0
    Ic1pPAL   = 0
    Ic10pPAL  = 0
    Icount3   = 0
    Isucc     = 1
    Isuc2     = 0
    sumJred   = 0d0
    sumTime   = 0d0
    sumGRBexo = 0d0
    sumJman   = 0d0
    sumJsbd   = 0d0
!
     Time = -600d6
    sTime = -600d6
    Ij  = 0
    Ij2 = 0
    Ioutput = 0

    Return
  End Subroutine INIT
!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!
  Subroutine PHOTOCHEM(pO2,pCH4,phi_o2,phi_ch4,kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,kch4ox)
    USE Constants
    Implicit none
    double precision:: pO2(2),pCH4(2),phi_o2,phi_ch4
    double precision:: kch4ox_6,kch4ox_5,kch4ox_4,kch4ox_3,kch4ox_23,kch4ox

    phi_o2 = dlog10(pO2(1)/0.987d0)   ! 1 bar = 0.987 atm
    If(phi_o2 <= -9d0) then
      kch4ox_6  = c0l6 + c1l6*phi_o2 + c2l6*phi_o2*phi_o2 + c3l6*phi_o2**3d0     &
                       + c4l6*phi_o2**4d0 + c5l6*phi_o2**5d0 + c6l6*phi_o2**6d0
      kch4ox_5  = c0l5 + c1l5*phi_o2 + c2l5*phi_o2*phi_o2 + c3l5*phi_o2**3d0     &
                       + c4l5*phi_o2**4d0 + c5l5*phi_o2**5d0 + c6l5*phi_o2**6d0
      kch4ox_4  = c0l4 + c1l4*phi_o2 + c2l4*phi_o2*phi_o2 + c3l4*phi_o2**3d0     &
                       + c4l4*phi_o2**4d0 + c5l4*phi_o2**5d0 + c6l4*phi_o2**6d0
      kch4ox_3  = c0l3 + c1l3*phi_o2 + c2l3*phi_o2*phi_o2 + c3l3*phi_o2**3d0     &
                       + c4l3*phi_o2**4d0 + c5l3*phi_o2**5d0 + c6l3*phi_o2**6d0
      kch4ox_23 = c0l23 + c1l23*phi_o2 + c2l23*phi_o2*phi_o2 + c3l23*phi_o2**3d0 &
                        + c4l23*phi_o2**4d0 + c5l23*phi_o2**5d0 + c6l23*phi_o2**6d0
    Else If((phi_o2 > -9d0).and.(phi_o2 <= -3d0)) then
      kch4ox_6  = c0m6 + c1m6*phi_o2 + c2m6*phi_o2*phi_o2 + c3m6*phi_o2**3d0     &
                       + c4m6*phi_o2**4d0 + c5m6*phi_o2**5d0 + c6m6*phi_o2**6d0
      kch4ox_5  = c0m5 + c1m5*phi_o2 + c2m5*phi_o2*phi_o2 + c3m5*phi_o2**3d0     &
                       + c4m5*phi_o2**4d0 + c5m5*phi_o2**5d0 + c6m5*phi_o2**6d0
      kch4ox_4  = c0m4 + c1m4*phi_o2 + c2m4*phi_o2*phi_o2 + c3m4*phi_o2**3d0     &
                       + c4m4*phi_o2**4d0 + c5m4*phi_o2**5d0 + c6m4*phi_o2**6d0
      kch4ox_3  = c0m3 + c1m3*phi_o2 + c2m3*phi_o2*phi_o2 + c3m3*phi_o2**3d0     &
                       + c4m3*phi_o2**4d0 + c5m3*phi_o2**5d0 + c6m3*phi_o2**6d0
      kch4ox_23 = c0m23 + c1m23*phi_o2 + c2m23*phi_o2*phi_o2 + c3m23*phi_o2**3d0 &
                        + c4m23*phi_o2**4d0 + c5m23*phi_o2**5d0 + c6m23*phi_o2**6d0
    Else
      kch4ox_6  = c0h6 + c1h6*phi_o2 + c2h6*phi_o2*phi_o2 + c3h6*phi_o2**3d0
      kch4ox_5  = c0h5 + c1h5*phi_o2 + c2h5*phi_o2*phi_o2 + c3h5*phi_o2**3d0
      kch4ox_4  = c0h4 + c1h4*phi_o2 + c2h4*phi_o2*phi_o2 + c3h4*phi_o2**3d0
      kch4ox_3  = c0h3 + c1h3*phi_o2 + c2h3*phi_o2*phi_o2 + c3h3*phi_o2**3d0
      kch4ox_23 = c0h23 + c1h23*phi_o2 + c2h23*phi_o2*phi_o2 + c3h23*phi_o2**3d0
    End If
    phi_ch4 = dlog10(pCH4(1)/0.987d0)
    If(phi_ch4 <= -6d0) then
      kch4ox = kch4ox_6
    Else If((phi_ch4 > -6d0).and.(phi_ch4 <= -5d0)) then
      kch4ox = kch4ox_6 + (kch4ox_5 - kch4ox_6) * (phi_ch4 + 6d0)
    Else If((phi_ch4 > -5d0).and.(phi_ch4 <= -4d0)) then
      kch4ox = kch4ox_5 + (kch4ox_4 - kch4ox_5) * (phi_ch4 + 5d0)
    Else If((phi_ch4 > -4d0).and.(phi_ch4 <= -3d0)) then
      kch4ox = kch4ox_4 + (kch4ox_3 - kch4ox_4) * (phi_ch4 + 4d0)
    Else If((phi_ch4 > -3d0).and.(phi_ch4 <= dlog10(2d-3))) then
      kch4ox = kch4ox_3 + (kch4ox_23 - kch4ox_3) * (phi_ch4 + 3d0) / (dlog10(2d-3)+3d0)
    Else
      kch4ox = kch4ox_23
    End If
    kch4ox = 10d0**kch4ox
!
    Return
  End Subroutine PHOTOCHEM
!
!----------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------!