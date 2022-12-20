SET(src_path "${CMAKE_CURRENT_LIST_DIR}/NETLIB")
LIST(APPEND l95_src
${src_path}/la_sgesv.F90 ${src_path}/la_sgesv1.F90 ${src_path}/la_sgesvx.F90 ${src_path}/la_sgesvx1.F90
${src_path}/la_sgetrf.F90 ${src_path}/la_slagge.F90 ${src_path}/la_slange.F90 ${src_path}/la_slange1.F90
${src_path}/la_sgetri.F90 ${src_path}/la_sgetrs.F90 ${src_path}/la_sgetrs1.F90 ${src_path}/la_sgeequ.F90
${src_path}/la_sgerfs.F90 ${src_path}/la_sgerfs1.F90 ${src_path}/la_ssytrd.F90 ${src_path}/la_ssygst.F90
${src_path}/la_sorgtr.F90 ${src_path}/la_spotrf.F90 ${src_path}/la_sgbtrf.F90 ${src_path}/la_slamch.F90
${src_path}/la_sgbsv.F90 ${src_path}/la_sgbsv1.F90 ${src_path}/la_sgbsvx.F90 ${src_path}/la_sgbsvx1.F90
${src_path}/la_sgtsv.F90 ${src_path}/la_sgtsv1.F90 ${src_path}/la_sgtsvx.F90 ${src_path}/la_sgtsvx1.F90
${src_path}/la_sposv.F90 ${src_path}/la_sposv1.F90 ${src_path}/la_sposvx.F90 ${src_path}/la_sposvx1.F90
${src_path}/la_sppsv.F90 ${src_path}/la_sppsv1.F90 ${src_path}/la_sppsvx.F90 ${src_path}/la_sppsvx1.F90
${src_path}/la_spbsv.F90 ${src_path}/la_spbsv1.F90 ${src_path}/la_spbsvx.F90 ${src_path}/la_spbsvx1.F90
${src_path}/la_sptsv.F90 ${src_path}/la_sptsv1.F90 ${src_path}/la_sptsvx.F90 ${src_path}/la_sptsvx1.F90
${src_path}/la_ssysv.F90 ${src_path}/la_ssysv1.F90 ${src_path}/la_ssysvx.F90 ${src_path}/la_ssysvx1.F90
${src_path}/la_sspsv.F90 ${src_path}/la_sspsv1.F90 ${src_path}/la_sspsvx.F90 ${src_path}/la_sspsvx1.F90
${src_path}/la_sgels.F90 ${src_path}/la_sgels1.F90 ${src_path}/la_sgelsx.F90 ${src_path}/la_sgelsx1.F90
${src_path}/la_sgelss.F90 ${src_path}/la_sgelss1.F90 ${src_path}/la_sgglse.F90 ${src_path}/la_sggglm.F90
${src_path}/la_ssyev.F90 ${src_path}/la_ssyevd.F90 ${src_path}/la_ssyevx.F90 ${src_path}/la_sspev.F90
${src_path}/la_sspevd.F90 ${src_path}/la_sspevx.F90 ${src_path}/la_ssbev.F90 ${src_path}/la_ssbevd.F90
${src_path}/la_ssbevx.F90 ${src_path}/la_sstev.F90 ${src_path}/la_sstevd.F90 ${src_path}/la_sstevx.F90
${src_path}/la_sgees.F90 ${src_path}/la_sgeesx.F90 ${src_path}/la_sgeev.F90 ${src_path}/la_sgeevx.F90
${src_path}/la_sgesvd.F90 ${src_path}/la_ssygv.F90 ${src_path}/la_sspgv.F90 ${src_path}/la_ssbgv.F90
${src_path}/la_sgegs.F90 ${src_path}/la_sgegv.F90 ${src_path}/la_sggsvd.F90 ${src_path}/la_sgelsy.F90
${src_path}/la_sgelsy1.F90 ${src_path}/la_sspgvd.F90 ${src_path}/la_ssbgvd.F90 ${src_path}/la_ssygvd.F90
${src_path}/la_ssbgvx.F90 ${src_path}/la_sspgvx.F90 ${src_path}/la_sggev.F90 ${src_path}/la_sgges.F90
${src_path}/la_sstevr.F90 ${src_path}/la_sgelsd.F90 ${src_path}/la_sgelsd1.F90 ${src_path}/la_ssyevr.F90
${src_path}/la_sggevx.F90 ${src_path}/la_ssygvx.F90 ${src_path}/la_sggesx.F90 ${src_path}/la_sgesdd.F90
${src_path}/la_ssytrf.F90 ${src_path}/la_slacpy.F90 ${src_path}/la_slapmr.F90 ${src_path}/la_slapmt.F90
${src_path}/la_ssyswapr.F90
)

LIST(APPEND l95_src
${src_path}/la_dgesv.F90 ${src_path}/la_dgesv1.F90 ${src_path}/la_dgesvx.F90 ${src_path}/la_dgesvx1.F90
${src_path}/la_dgetrf.F90 ${src_path}/la_dlagge.F90 ${src_path}/la_dlange.F90 ${src_path}/la_dlange1.F90
${src_path}/la_dgetri.F90 ${src_path}/la_dgetrs.F90 ${src_path}/la_dgetrs1.F90 ${src_path}/la_dgeequ.F90
${src_path}/la_dgerfs.F90 ${src_path}/la_dgerfs1.F90 ${src_path}/la_dsytrd.F90 ${src_path}/la_dsygst.F90
${src_path}/la_dorgtr.F90 ${src_path}/la_dpotrf.F90 ${src_path}/la_dgbtrf.F90 ${src_path}/la_dlamch.F90
${src_path}/la_dgbsv.F90 ${src_path}/la_dgbsv1.F90 ${src_path}/la_dgbsvx.F90 ${src_path}/la_dgbsvx1.F90
${src_path}/la_dgtsv.F90 ${src_path}/la_dgtsv1.F90 ${src_path}/la_dgtsvx.F90 ${src_path}/la_dgtsvx1.F90
${src_path}/la_dposv.F90 ${src_path}/la_dposv1.F90 ${src_path}/la_dposvx.F90 ${src_path}/la_dposvx1.F90
${src_path}/la_dppsv.F90 ${src_path}/la_dppsv1.F90 ${src_path}/la_dppsvx.F90 ${src_path}/la_dppsvx1.F90
${src_path}/la_dpbsv.F90 ${src_path}/la_dpbsv1.F90 ${src_path}/la_dpbsvx.F90 ${src_path}/la_dpbsvx1.F90
${src_path}/la_dptsv.F90 ${src_path}/la_dptsv1.F90 ${src_path}/la_dptsvx.F90 ${src_path}/la_dptsvx1.F90
${src_path}/la_dsysv.F90 ${src_path}/la_dsysv1.F90 ${src_path}/la_dsysvx.F90 ${src_path}/la_dsysvx1.F90
${src_path}/la_dspsv.F90 ${src_path}/la_dspsv1.F90 ${src_path}/la_dspsvx.F90 ${src_path}/la_dspsvx1.F90
${src_path}/la_dgels.F90 ${src_path}/la_dgels1.F90 ${src_path}/la_dgelsx.F90 ${src_path}/la_dgelsx1.F90
${src_path}/la_dgelss.F90 ${src_path}/la_dgelss1.F90 ${src_path}/la_dgglse.F90 ${src_path}/la_dggglm.F90
${src_path}/la_dsyev.F90 ${src_path}/la_dsyevd.F90 ${src_path}/la_dsyevx.F90 ${src_path}/la_dspev.F90
${src_path}/la_dspevd.F90 ${src_path}/la_dspevx.F90 ${src_path}/la_dsbev.F90 ${src_path}/la_dsbevd.F90
${src_path}/la_dsbevx.F90 ${src_path}/la_dstev.F90 ${src_path}/la_dstevd.F90 ${src_path}/la_dstevx.F90
${src_path}/la_dgees.F90 ${src_path}/la_dgeesx.F90 ${src_path}/la_dgeev.F90 ${src_path}/la_dgeevx.F90
${src_path}/la_dgesvd.F90 ${src_path}/la_dsygv.F90 ${src_path}/la_dspgv.F90 ${src_path}/la_dsbgv.F90
${src_path}/la_dgegs.F90 ${src_path}/la_dgegv.F90 ${src_path}/la_dggsvd.F90 ${src_path}/la_dgelsy.F90
${src_path}/la_dgelsy1.F90 ${src_path}/la_dspgvd.F90 ${src_path}/la_dsbgvd.F90 ${src_path}/la_dsygvd.F90
${src_path}/la_dsbgvx.F90 ${src_path}/la_dspgvx.F90 ${src_path}/la_dggev.F90 ${src_path}/la_dgges.F90
${src_path}/la_dstevr.F90 ${src_path}/la_dgelsd.F90 ${src_path}/la_dgelsd1.F90 ${src_path}/la_dsyevr.F90
${src_path}/la_dggevx.F90 ${src_path}/la_dsygvx.F90 ${src_path}/la_dggesx.F90 ${src_path}/la_dgesdd.F90
${src_path}/la_dsytrf.F90 ${src_path}/la_dlacpy.F90 ${src_path}/la_dlapmr.F90 ${src_path}/la_dlapmt.F90
${src_path}/la_dsyswapr.F90
)

LIST(APPEND l95_src
${src_path}/la_cgesv.F90 ${src_path}/la_cgesv1.F90 ${src_path}/la_cgesvx.F90 ${src_path}/la_cgesvx1.F90
${src_path}/la_cgetrf.F90 ${src_path}/la_clagge.F90 ${src_path}/la_clange.F90 ${src_path}/la_clange1.F90
${src_path}/la_cgetri.F90 ${src_path}/la_cgetrs.F90 ${src_path}/la_cgetrs1.F90 ${src_path}/la_cgeequ.F90
${src_path}/la_cgerfs.F90 ${src_path}/la_cgerfs1.F90 ${src_path}/la_chetrd.F90 ${src_path}/la_chegst.F90
${src_path}/la_cungtr.F90 ${src_path}/la_cpotrf.F90 ${src_path}/la_cgbtrf.F90 ${src_path}/la_cgbsv.F90
${src_path}/la_cgbsv1.F90 ${src_path}/la_cgbsvx.F90 ${src_path}/la_cgbsvx1.F90 ${src_path}/la_cgtsv.F90
${src_path}/la_cgtsv1.F90 ${src_path}/la_cgtsvx.F90 ${src_path}/la_cgtsvx1.F90 ${src_path}/la_cposv.F90
${src_path}/la_cposv1.F90 ${src_path}/la_cposvx.F90 ${src_path}/la_cposvx1.F90 ${src_path}/la_cppsv.F90
${src_path}/la_cppsv1.F90 ${src_path}/la_cppsvx.F90 ${src_path}/la_cppsvx1.F90 ${src_path}/la_cpbsv.F90
${src_path}/la_cpbsv1.F90 ${src_path}/la_cpbsvx.F90 ${src_path}/la_cpbsvx1.F90 ${src_path}/la_cptsv.F90
${src_path}/la_cptsv1.F90 ${src_path}/la_cptsvx.F90 ${src_path}/la_cptsvx1.F90 ${src_path}/la_csysv.F90
${src_path}/la_csysv1.F90 ${src_path}/la_csysvx.F90 ${src_path}/la_csysvx1.F90 ${src_path}/la_cspsv.F90
${src_path}/la_cspsv1.F90 ${src_path}/la_cspsvx.F90 ${src_path}/la_cspsvx1.F90 ${src_path}/la_chesv.F90
${src_path}/la_chesv1.F90 ${src_path}/la_chesvx.F90 ${src_path}/la_chesvx1.F90 ${src_path}/la_chpsv.F90
${src_path}/la_chpsv1.F90 ${src_path}/la_chpsvx.F90 ${src_path}/la_chpsvx1.F90 ${src_path}/la_cgels.F90
${src_path}/la_cgels1.F90 ${src_path}/la_cgelsx.F90 ${src_path}/la_cgelsx1.F90 ${src_path}/la_cgelss.F90
${src_path}/la_cgelss1.F90 ${src_path}/la_cgglse.F90 ${src_path}/la_cggglm.F90 ${src_path}/la_cheev.F90
${src_path}/la_cheevd.F90 ${src_path}/la_cheevx.F90 ${src_path}/la_chpev.F90 ${src_path}/la_chpevd.F90
${src_path}/la_chpevx.F90 ${src_path}/la_chbev.F90 ${src_path}/la_chbevd.F90 ${src_path}/la_chbevx.F90
${src_path}/la_chbgvx.F90 ${src_path}/la_chpgvx.F90 ${src_path}/la_cgees.F90 ${src_path}/la_cgeesx.F90
${src_path}/la_cgeev.F90 ${src_path}/la_cgeevx.F90 ${src_path}/la_cgesvd.F90 ${src_path}/la_chegv.F90
${src_path}/la_chpgv.F90 ${src_path}/la_chbgv.F90 ${src_path}/la_cgegs.F90 ${src_path}/la_cgegv.F90
${src_path}/la_cggsvd.F90 ${src_path}/la_cgelsy.F90 ${src_path}/la_cgelsy1.F90 ${src_path}/la_chpgvd.F90
${src_path}/la_chbgvd.F90 ${src_path}/la_chegvd.F90 ${src_path}/la_cggev.F90 ${src_path}/la_cgges.F90
${src_path}/la_cgelsd.F90 ${src_path}/la_cgelsd1.F90 ${src_path}/la_cheevr.F90 ${src_path}/la_cggevx.F90
${src_path}/la_chegvx.F90 ${src_path}/la_cggesx.F90 ${src_path}/la_cgesdd.F90 ${src_path}/la_csytrf.F90
${src_path}/la_clacpy.F90 ${src_path}/la_clapmr.F90 ${src_path}/la_clapmt.F90 ${src_path}/la_csyswapr.F90
)

LIST(APPEND l95_src
${src_path}/la_zgesv.F90 ${src_path}/la_zgesv1.F90 ${src_path}/la_zgesvx.F90 ${src_path}/la_zgesvx1.F90
${src_path}/la_zgetrf.F90 ${src_path}/la_zlagge.F90 ${src_path}/la_zlange.F90 ${src_path}/la_zlange1.F90
${src_path}/la_zgetri.F90 ${src_path}/la_zgetrs.F90 ${src_path}/la_zgetrs1.F90 ${src_path}/la_zgeequ.F90
${src_path}/la_zgerfs.F90 ${src_path}/la_zgerfs1.F90 ${src_path}/la_zhetrd.F90 ${src_path}/la_zhegst.F90
${src_path}/la_zungtr.F90 ${src_path}/la_zpotrf.F90 ${src_path}/la_zgbtrf.F90 ${src_path}/la_zgbsv.F90
${src_path}/la_zgbsv1.F90 ${src_path}/la_zgbsvx.F90 ${src_path}/la_zgbsvx1.F90 ${src_path}/la_zgtsv.F90
${src_path}/la_zgtsv1.F90 ${src_path}/la_zgtsvx.F90 ${src_path}/la_zgtsvx1.F90 ${src_path}/la_zposv.F90
${src_path}/la_zposv1.F90 ${src_path}/la_zposvx.F90 ${src_path}/la_zposvx1.F90 ${src_path}/la_zppsv.F90
${src_path}/la_zppsv1.F90 ${src_path}/la_zppsvx.F90 ${src_path}/la_zppsvx1.F90 ${src_path}/la_zpbsv.F90
${src_path}/la_zpbsv1.F90 ${src_path}/la_zpbsvx.F90 ${src_path}/la_zpbsvx1.F90 ${src_path}/la_zptsv.F90
${src_path}/la_zptsv1.F90 ${src_path}/la_zptsvx.F90 ${src_path}/la_zptsvx1.F90 ${src_path}/la_zsysv.F90
${src_path}/la_zsysv1.F90 ${src_path}/la_zsysvx.F90 ${src_path}/la_zsysvx1.F90 ${src_path}/la_zspsv.F90
${src_path}/la_zspsv1.F90 ${src_path}/la_zspsvx.F90 ${src_path}/la_zspsvx1.F90 ${src_path}/la_zhesv.F90
${src_path}/la_zhesv1.F90 ${src_path}/la_zhesvx.F90 ${src_path}/la_zhesvx1.F90 ${src_path}/la_zhpsv.F90
${src_path}/la_zhpsv1.F90 ${src_path}/la_zhpsvx.F90 ${src_path}/la_zhpsvx1.F90 ${src_path}/la_zgels.F90
${src_path}/la_zgels1.F90 ${src_path}/la_zgelsx.F90 ${src_path}/la_zgelsx1.F90 ${src_path}/la_zgelss.F90
${src_path}/la_zgelss1.F90 ${src_path}/la_zgglse.F90 ${src_path}/la_zggglm.F90 ${src_path}/la_zheev.F90
${src_path}/la_zheevd.F90 ${src_path}/la_zheevx.F90 ${src_path}/la_zhpev.F90 ${src_path}/la_zhpevd.F90
${src_path}/la_zhpevx.F90 ${src_path}/la_zhbev.F90 ${src_path}/la_zhbevd.F90 ${src_path}/la_zhbevx.F90
${src_path}/la_zhbgvx.F90 ${src_path}/la_zhpgvx.F90 ${src_path}/la_zgees.F90 ${src_path}/la_zgeesx.F90
${src_path}/la_zgeev.F90 ${src_path}/la_zgeevx.F90 ${src_path}/la_zgesvd.F90 ${src_path}/la_zhegv.F90
${src_path}/la_zhpgv.F90 ${src_path}/la_zhbgv.F90 ${src_path}/la_zgegs.F90 ${src_path}/la_zgegv.F90
${src_path}/la_zggsvd.F90 ${src_path}/la_zgelsy.F90 ${src_path}/la_zgelsy1.F90 ${src_path}/la_zhpgvd.F90
${src_path}/la_zhbgvd.F90 ${src_path}/la_zhegvd.F90 ${src_path}/la_zggev.F90 ${src_path}/la_zgges.F90
${src_path}/la_zgelsd.F90 ${src_path}/la_zgelsd1.F90 ${src_path}/la_zheevr.F90 ${src_path}/la_zggevx.F90
${src_path}/la_zhegvx.F90 ${src_path}/la_zggesx.F90 ${src_path}/la_zgesdd.F90 ${src_path}/la_zsytrf.F90
${src_path}/la_zlacpy.F90 ${src_path}/la_zlapmr.F90 ${src_path}/la_zlapmt.F90 ${src_path}/la_zsyswapr.F90
)

TARGET_SOURCES(

  ${PROJECT_NAME} PRIVATE
  ${src_path}/la_auxmod.F90
  ${src_path}/la_erinfo.F90
  ${src_path}/la_ws_gels.F90
  ${src_path}/la_ws_gelss.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_BD_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_GB_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_GE_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_GG_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_GT_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_SY_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_HB_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_HE_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_HG_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_HP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_HS_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_OP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_OR_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_PB_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_PO_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_PP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_PT_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_SB_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_SP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_ST_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_TB_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_TG_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_TP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_TR_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_TZ_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_UN_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_UP_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_LA_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F77/F77_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F95/F95_GE_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F95/F95_SY_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F95/F95_LA_LAPACK.F90
  ${CMAKE_CURRENT_LIST_DIR}/F95/F95_LAPACK.F90
  ${l95_src}
)

# TARGET_SOURCES(
#   ${PROJECT_NAME} PRIVATE
#   ${l95_src})
