program main_AutoTest

    use systemUt_RF
    use charFunctions
    use constants_Auto
    use fileManag_Auto

    implicit none

    !USER
    integer, parameter :: cluster = FUSION !IGLOO, OXIGEN, LOCAL_MAC, FUSION, S_DUMONT
    
    double precision, dimension(NDIM,NDIM), parameter :: xMax_init = reshape([1d0, 0d0, 0d0, &
                                                                              1d0, 1d0, 0d0, &
                                                                              1d0, 1d0, 1d0], &
                                                                              shape(xMax_init))  
    double precision, dimension(NDIM,NDIM), parameter :: xMax_mult = reshape([2d0, 0d0, 0d0, &
                                                                              2d0, 2d0, 0d0, &
                                                                              2d0, 2d0, 2d0], &
                                                                              shape(xMax_mult))
    double precision, dimension(NDIM,NDIM), parameter :: xMax_max  = reshape([1024d0,    0d0,    0d0, &
                                                                              1024d0, 1024d0,    0d0, &
                                                                              1024d0, 1024d0, 1024d0], &
                                                                              shape(xMax_max))   
    integer, dimension(NDIM), parameter :: nProc_init = [1, 1, 1]
    integer, dimension(NDIM), parameter :: nProc_mult = [2, 2, 2]
    integer, dimension(NDIM), parameter :: nProc_max  = [512, 512, 512]
    integer :: nRuns = 1 !How many times each iteration
    logical, dimension(NDIM) :: activeDim      = [.false., .false., .true.] !1D, 2D and 3D
    logical, dimension(4) :: activeMethod   = [.false., .false. , .false. , .true.] !Isotropic, Shinozuka, Randomization and FFT
    logical, dimension(2) :: activeApproach = [.true. , .false.] !Global, Local
    character(len=50), parameter :: res_folder = "WEAK"

    !COMPUTATION
    integer :: memPerNTerm = 1000 !mb
    integer :: NTerm = 1000000000 !Terms
    integer :: memBase = 2 !mb
    double precision :: timePerTerm = 1.0D-7 !s
    double precision, dimension(NDIM), parameter :: xMaxBase = [1.0D0, 1.0D0, 1.0D0]
    integer         , dimension(NDIM) :: nIter !number of iterations in each dimension
    integer         , dimension(NDIM) :: iterBase !initial iteration (multiply xMaxBase)
    
    !GENERATION
    integer :: Nmc = 1
    integer :: corrMod = cm_GAUSSIAN
    integer :: margiFirst = fom_GAUSSIAN
    double precision   :: fieldAvg = 0.0D0, fieldVar=1.0D0;
    double precision, dimension(:), allocatable :: corrL, overlap;
    integer, dimension(3) :: ignore_Till_Iteration = [0, 0, 0];
    integer :: method !1 for Isotropic, 2 for Shinozuka, 3 for Randomization, 4 for FFT
    integer :: indep
    double precision :: corrLBase = 1.0D0
    double precision :: overlapBase = 5.0D0
    integer :: pointsPerCorrLBase = 5
    integer :: seedStart = -1
    integer :: dimMin = 1, dimMax = 3
    integer :: methodMin = 1, methodMax = 4
    integer :: independent !0 = false, 1 = true (other numbers will be considered as false)

    integer :: localizationLevel = 1
    integer, dimension(:), allocatable :: nFields

    !GENERAL NAMES AND PATHS
    character(len=50) :: dim_folder
    character(len=50) :: method_folder
    character(len=50) :: it_folder
    character(len=50) :: Lchar
    character(len=200) :: res_path
    character(len=200) :: dim_path
    character(len=200) :: method_path
    character(len=tSize) :: it_path
    character(len=tSize) :: runAll_path, cleanAll_path, listAll_path
    character(len=50) :: genName = "gen_input"
    character(len=50) :: meshName = "mesh_input"
    character(len=10) :: methodTxt
    character(len=1) :: indepChar
    character(len=tSize) :: full_path
    character(len=tSize) :: run_path
    character(len=tSize) :: temp_path
    character(len=200) :: iterChar

    !MESH
    character (len=15) :: meshMod  = "automatic";
    double precision, dimension(:), allocatable :: xMax, xMin;
    integer, dimension(:), allocatable :: pointsPerCorrL

    !QUEUE MANAGEMENT
    integer :: proc_per_chunk_Max, mem_per_chunk_Max, n_chunk_Max
    integer :: nProcsTotal
    character(len=8) :: wallTime
    character(len=20) :: queue, errorQ = "No_q"
    integer :: memMax = 140000
    integer :: procTotMax = 512
    integer(kind=8) :: memTotal !mb
    integer(kind=8) :: timeTotal !s
    integer :: maxProcReq !maximum of Processors Requested
    logical :: qFound
    !PBS
    character(len=50) :: pbsName
    character(len=50) :: jobName
    character(len=50) :: outName

    !LOCAL
    integer :: fileId = 15
    integer :: compiler = 2 !1 for gfortran and 2 for ifort
    logical :: dirExists
    integer(kind=8) :: xNTotal, kNTotal
    integer :: memPerProc, memPerChunk = -1
    integer :: timePerProc !s
    integer :: runAll_Id = 100, cleanAll_Id = 101, listAll_Id=102
    character(len=200) :: format
    double precision :: kAdjust    = 1.0D0 !"kNStep minimum" multiplier
    double precision :: periodMult = 1.1D0 !"range" multiplier
    integer :: i, it, secur = 0, pos
    integer :: d
    logical :: initial


    !FOR FILE CREATION
    character(len=200) :: results_path
    integer :: nProcsPerChunk
    integer :: nChunks

    select case (cluster)
        case(IGLOO)
            buildPath = "/home/carvalhol/Projects/RANDOM_FIELD/build"
            proc_per_chunk_Max = 12
            mem_per_chunk_Max = 24000
            n_chunk_Max = 56
            queue = "iceq"
            wallTime = "04:00:00"
        case(LOCAL_MAC)
            buildPath = "/Users/carvalhol/Desktop/GITs/RANDOM_FIELD/build"
            proc_per_chunk_Max = 1
            mem_per_chunk_Max = 4000
            n_chunk_Max = 1
        case(OXIGEN)
            buildPath = "/home/abreul/RF/Novo/build"
            proc_per_chunk_Max = 24
            mem_per_chunk_Max = 64000
            n_chunk_Max = 80000/24
        case(FUSION)
            buildPath = "/home/carvalhol/RANDOM_FIELD/build"
            proc_per_chunk_Max = 24
            mem_per_chunk_Max = 24000
            n_chunk_Max = 56
            queue = "haswellq"
            wallTime = "04:00:00"
        case(S_DUMONT)
            buildPath = "/home/carvalhol/RANDOM_FIELD/build"
            proc_per_chunk_Max = 24
            mem_per_chunk_Max = 24000
            n_chunk_Max = 56
    end select
    
    execPath  = trim(adjustL(buildPath))//"/randomField.exe"
    exec2Path = trim(adjustL(buildPath))//"/statistics.exe"

    write(*,*) "Running on: "
    call system("pwd")

    !nIter    -> Number of iterations (define in 1D, 2D and 3D)
    where(nProc_mult==1)
        nIter(:) = log(xMax_max(1,:)/xMax_init(1,:))/log(xMax_mult(1,:))
    elsewhere
        nIter(:) = nint(log(dble(nProc_max(:))/dble(nProc_init(:)))/log(dble(nProc_mult(:)))) 
    end where
    write(*,*) "nIter = ", nIter

    !Global folders and files creation
    call delete_folder(res_folder, basePath)
    call create_folder(res_folder, basePath)

    !Global exigences
   

!    if(cluster == IGLOO) then
!        if(singleProc) then
!            !queue = "uvq"
!            proc_per_chunk_Max = nProc_single
!            mem_per_chunk_Max = 125000
!            n_chunk_Max = 1
!            wallTime = "24:00:00"
!            queue = "iceq"
!            !wallTime = "02:00:00" !FOR TESTS
!        else if (maxProcReq < 385) then
!            queue = "icexq"
!            proc_per_chunk_Max = 24
!            mem_per_chunk_Max = 32000
!            n_chunk_Max = 16
!            wallTime = "04:00:00"
!        else
!            queue = "iceq"
!            proc_per_chunk_Max = 12
!            mem_per_chunk_Max = 24000
!            n_chunk_Max = 56
!            wallTime = "04:00:00"
!        end if
!
!    else if (cluster == OXIGEN) then
!        proc_per_chunk_Max = 24
!        mem_per_chunk_Max = 64000
!        n_chunk_Max = 80000/24
!        wallTime = "04:00:00" !Max "24:00:00"
!    else if (cluster == LOCAL_MAC) then
!        proc_per_chunk_Max = 16
!        mem_per_chunk_Max = 80000
!        n_chunk_Max = 1000
!        wallTime = "04:00:00"
!    end if
!
    do indep = 1, size(activeApproach)

        !independent = 0
        if(.not. activeApproach(indep)) cycle

        independent = indep - 1
        indepChar = "g" !g for Global
        if (independent == 1) indepChar = "l" !l for Local (using localization)

        do d = 1, size(activeDim)

            if(allocated(nFields)) deallocate(nFields)
            allocate(nFields(d))
            if(.not. activeDim(d)) cycle

            !runAll file creation
            !initial = .true.
            runAll_path = string_join_many(basePath, res_folder, &
                "/runAll_",numb2String(d),"D-",indepChar,".sh")
            write(*,*) "runAll_path = ", runAll_path
            open (unit = runAll_Id , file = runAll_path, action = 'write')
            cleanAll_path = string_join_many(basePath, res_folder, &
                "/cleanAll_",numb2String(d),"D-",indepChar,".sh")
            listAll_path = string_join_many(basePath, res_folder, &
                "/listAll_",numb2String(d),"D-",indepChar,".sh")
            write(*,*) "cleanAll_path = ", runAll_path
            open (unit = cleanAll_Id , file = cleanAll_path, action = 'write')
            open (unit = listAll_Id , file = listAll_path, action = 'write')
            write(runAll_Id,"(A)") "#!/bin/bash"
            write(runAll_Id,"(A)") ""
            write(runAll_Id,"(A)") "clear"
            write(runAll_Id,"(A)")
            write(runAll_Id,"(A)") "startTime=`date -u`"
            write(runAll_Id,"(A)")
            write(runAll_Id,"(A)") trim(string_join_many("(cd "//buildPath, "; make all)"))
            write(runAll_Id,"(A)")
            write(runAll_Id,"(A)") "nRuns="//numb2String(nRuns)
            write(runAll_Id,"(A)") "Run_RF=1"
            write(runAll_Id,"(A)") "Run_Stat=1"
            write(runAll_Id,"(A)") "sleep_time=0"
            write(runAll_Id,"(A)") "for i in {1..1}"
            write(runAll_Id,"(A)") "do"
            write(runAll_Id,"(A)") '   echo "Running $i"'
            write(listAll_Id,"(A)") 'res="res"'
            write(cleanAll_Id,"(A)") 'res="res"'

            do method = 1, size(activeMethod)

                if(.not. activeMethod(method)) cycle

                if(method == ISOTROPIC .and. d == 1) cycle !No isotropic 1D

                if(method == ISOTROPIC) methodTxt = "ISO  "
                if(method == SHINOZUKA) methodTxt = "SHI"
                if(method == RANDOMIZATION) methodTxt = "RAN"
                if(method == FFT) methodTxt = "FFT"

                methodTxt = trim(adjustl(string_join_many(methodTxt,"-",indepChar)))

                !Creating iterations
                do it = 1, nIter(d)

                    !Creating folder
                    it_path   = string_join_many(basePath,res_folder,"/",     &
                        numb2String(d),"D/", methodTxt)
                    it_folder = numb2String(it, 3)
                    call create_folder(it_folder, it_path)
                    full_path = string_join_many(it_path,"/",it_folder)

                    !Setting Case properties
                    !GENERATION FILE
                    call set_vec(corrL, [(corrLBase, i=1, d)])
                    call set_vec(overlap, [(overlapBase, i=1, d)])

                    !MESH FILE
                    call set_vec(xMax, [(1.0D0, i=1, d)]) !Starts xMax in 1
                    call set_vec(xMin, [(0.0D0, i=1, d)])  !Starts xMin in 0
                    call set_vec_Int(pointsPerCorrL, [(pointsPerCorrLBase, i=1, d)])

                    xMax        = xMax_init(1:d,d)*(xMax_mult(1:d,d)**(dble(it-1)))
                    nProcsTotal = nProc_init(d)*nint(dble(nProc_mult(d))**(dble(it-1)))
                    nChunks = ceiling(dble(nProcsTotal)/dble(proc_per_chunk_Max))
                    nProcsPerChunk = nProcsTotal;
                    if(nChunks > 1) then
                        nProcsPerChunk = proc_per_chunk_Max
                        memPerChunk = mem_per_chunk_Max
                    else if(nChunks < 1) then
                        nChunks = 1
                        memPerChunk = ceiling(dble(mem_per_chunk_Max)*dble(nProcsTotal)/dble(proc_per_chunk_Max))    
                    end if
                    if(memPerChunk < 512) memPerChunk = 512

!                    !DEFINE NUMBER OF CLUSTERS
!                    if(singleProc) then
!                        nProcsTotal = nProc_single
!                        nProcsPerChunk = nProc_single
!                        nChunks = 1
!                        if(memPerChunk < 0) memPerChunk=mem_per_chunk_Max
!                    else
!                        nProcsTotal = 2**(it-1)
!                        if (cluster==2) then
!                            !OCCYGEN
!                            nChunks = ceiling(dble(nProcsTotal)/dble(proc_per_chunk_Max))
!                            memPerChunk = ceiling(dble(mem_per_chunk_Max)*dble(nProcsTotal)/dble(proc_per_chunk_Max))
!                            if(nChunks > 1) memPerChunk = mem_per_chunk_Max !proc_per_chunk_Max * memPerProc
!                            nProcsPerChunk = nProcsTotal;
!                            if(nChunks > 1) nProcsPerChunk = proc_per_chunk_Max
!                            if(nChunks < 1) nChunks = 1
!                            if(memPerChunk < 512) memPerChunk = 512
!                        else
!                            !IGLOO
!                            nChunks = ceiling(dble(nProcsTotal)/dble(proc_per_chunk_Max))
!                            memPerChunk = ceiling(dble(mem_per_chunk_Max)*dble(nProcsTotal)/dble(proc_per_chunk_Max))
!                            if(nChunks > 1) memPerChunk = mem_per_chunk_Max !proc_per_chunk_Max * memPerProc
!                            nProcsPerChunk = nProcsTotal;
!                            if(nChunks > 1) nProcsPerChunk = proc_per_chunk_Max
!                            if(nChunks < 1) nChunks = 1
!                            if(memPerChunk < 512) memPerChunk = 512
!                        end if
!                    end if

                    !Defining nFields
                    nFields(:) = 1

                    if(independent == 1) then
                        do i = 2, it
                            pos = 4-(mod(i-2,d) + 1)
                            nFields(pos) = nFields(pos) * 2
                        end do
                    end if


                !VERIFICATIONS
                if(nChunks > n_chunk_Max) then
                    write(*,*) "You asked for too many chunks"
                    write(*,*) "nChunks     = ", nChunks
                    write(*,*) "n_chunk_Max = ", n_chunk_Max
                    cycle
                end if
                if(memPerChunk > mem_per_chunk_Max) then
                    write(*,*) "You asked for too much memory"
                    write(*,*) "memPerChunk       = ", memPerChunk
                    write(*,*) "mem_per_chunk_Max = ", mem_per_chunk_Max
                    cycle
                end if

                call makeCase(nDim=d,                     &
                    Nmc=Nmc,                       &
                    corrMod=corrMod,               &
                    margiFirst=margiFirst,         &
                    corrL=corrL,                   &
                    fieldAvg=fieldAvg,             &
                    fieldVar=fieldVar,             &
                    method=method,                 &
                    seedStart=seedStart,           &
                    independent=independent,       &
                    overlap=overlap,               &
                    xMinGlob=xMin,                 &
                    xMaxGlob=xMax,                 &
                    pointsPerCorrL=pointsPerCorrL, &
                    nProcsTotal=nProcsTotal,       &
                    nProcsPerChunk=nProcsPerChunk, &
                    localizationLevel=localizationLevel, &
                    nFields = nFields,              &
                    nChunks=nChunks,               &
                    memPerChunk=memPerChunk,       &
                    queue=queue,                   &
                    wallTime=wallTime,             &
                    cluster=cluster,               &
                    folderPath=full_path,          &
                    runPath=run_path,              &
                    iter=it)

                temp_path = string_join_many("./", numb2String(d),"D/", methodTxt)

                if(it == 1) then
                    write(runAll_Id,"(A)") " "
                    write(runAll_Id,"(A)") " "
                    write(cleanAll_Id,"(A)") " "
                    write(cleanAll_Id,"(A)") " "
                    write(listAll_Id,"(A)") " "
                    write(listAll_Id,"(A)") " "
                end if
                if (cluster==1) then
                    write(runAll_Id,"(A)") trim(string_join_many("(cd "//trim(temp_path),"/",&
                                                trim(it_folder), &
                                                ";qsub -v Run_Stat=",&
                                                "$Run_Stat,Run_RF=$Run_RF,nRuns=$nRuns,sleep_time=$sleep_time run.pbs)"))
                                                !"; qsub run.pbs)"))
                else if (cluster==2) then
                    write(runAll_Id,"(A)") trim(string_join_many("(cd "//trim(temp_path),"/",&
                                                trim(it_folder), "; sbatch run.slurm)"))
                else
                    write(runAll_Id,"(A)") trim(string_join_many("(cd "//trim(temp_path),"/",&
                                                trim(it_folder), "; ./run.command)"))
                end if

                write(cleanAll_Id,"(A)") trim(string_join_many("(cd "//trim(temp_path),"/",&
                                              trim(it_folder), "; rm -r $res log_proc* stat_input mpd.hosts)"))
                write(listAll_Id,"(A)") trim(string_join_many("(cd "//trim(temp_path),"/",&
                                              trim(it_folder),"/$res", "; pwd; ls | wc -l)"))
                write(cleanAll_Id,"(A)") " "
                write(listAll_Id,"(A)") " "

                !initial = .false.
                end do !Tests

            end do !Methods

            !write(runAll_Id,"(A)") "sleep 1"
            write(runAll_Id,"(A)") "done"
            write(runAll_Id,"(A)")
            write(runAll_Id,"(A)") "endTime=`date -u`"
            write(runAll_Id,"(A)")
            write(runAll_Id,"(A)") 'echo "Start time $startTime"'
            write(runAll_Id,"(A)") 'echo "  End time $endTime"'
            write(runAll_Id,"(A)") ""
            if(cluster == 1) write(runAll_Id,"(A)") "qstat -u carvalhol"
            close (runAll_Id)
            close (cleanAll_Id)
            write(*,*) "-> runAll done"
            call system("chmod u+x "//trim(runAll_path))
            call system("chmod a+r "//trim(runAll_path))
            call system("chmod u+x "//trim(cleanAll_path))
            call system("chmod a+r "//trim(cleanAll_path))
            call system("chmod u+x "//trim(listAll_path))
            call system("chmod a+r "//trim(listAll_path))

        end do !Dimension

    end do !Independent

    if(allocated(pointsPerCorrL)) deallocate(pointsPerCorrL)
    if(allocated(corrL)) deallocate(corrL)
    if(allocated(overlap)) deallocate(overlap)
    if(allocated(xMin)) deallocate(xMin)
    if(allocated(xMax)) deallocate(xMax)
    if(allocated(nFields)) deallocate(nFields)

contains

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine set_vec(variable, values)
        !INPUT
        double precision, dimension(:) :: values
        double precision, dimension(:), allocatable :: variable

        if(allocated(variable)) deallocate(variable)
        allocate(variable(size(values)))
        variable = values

    end subroutine set_vec

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine set_vec_Int(variable, values)
        !INPUT
        integer, dimension(:) :: values
        integer, dimension(:), allocatable :: variable

        if(allocated(variable)) deallocate(variable)
        allocate(variable(size(values)))
        variable = values

    end subroutine set_vec_Int

!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    function find_WallTime(timeInSec) result(wallTimeText)
!
!        integer, intent(in) :: timeInSec
!        character(len=8)    :: wallTimeText
!
!        integer :: restantTime
!        integer :: compTime
!        integer :: div
!
!        wallTimeText = "--------"
!
!        if(timeInSec > 24*3600) then
!            wallTimeText = "--------"
!        else
!            restantTime = timeInSec
!
!            !Hours
!            div = 3600
!            compTime = restantTime/div
!            if(findCharSize(compTime) /= 2)then
!                wallTimeText(1:1) = "0"
!                wallTimeText(2:2) = numb2String(compTime)
!            else
!                wallTimeText(1:2) = numb2String(compTime)
!            end if
!            restantTime = timeInSec - compTime*div
!
!            !Minutes
!            div = 60
!            compTime = restantTime/div
!            wallTimeText(3:3) = ":"
!            if(findCharSize(compTime) /= 2)then
!                wallTimeText(4:4) = "0"
!                wallTimeText(5:5) = numb2String(compTime)
!            else
!                wallTimeText(4:5) = numb2String(compTime)
!            end if
!            restantTime = timeInSec - compTime*div
!
!            !Seconds
!            div = 1
!            compTime = restantTime/div
!            wallTimeText(6:6) = ":"
!            if(findCharSize(compTime) /= 2)then
!                wallTimeText(7:7) = "0"
!                wallTimeText(8:8) = numb2String(compTime)
!            else
!                wallTimeText(7:8) = numb2String(compTime)
!            end if
!
!            wallTimeText(8:8) = "0"
!        end if
!
!
!    end function find_WallTime
!
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    function choose_queue() result(qFound)
!
!        implicit none
!        integer :: m = 60, h = 3600, gb = 1024
!        logical :: qFound
!        integer :: memMaxPerChunk
!        integer :: nProcsMaxPerChunk
!
!        qFound = .false.
!        queue  = errorQ
!
!
!        !MONOCHUNK-----------------------------------------------------------------------------
!        !START TEST
!        if((timePerProc < 24*h .and. memPerProc < 100*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
!            .and. (.not. qFound)) then
!            queue = "uvq"
!            qFound = .true.
!        !Queue uvq
!        !
!        !    ressources limites :
!        !    + temps d'exécution < 24 heures
!        !    + nombre de coeurs < 12
!        !    + mémoire < 128 Gio
!        !    jobs cibles : jobs sequentiels et de type OpenMP nécessitant beaucoup de mémoire
!        !
!        end if
!        !END TEST
!        if((timePerProc < 24*h .and. memPerProc < 32*gb .and. nChunks == 1 .and. nProcsTotal < 24) &
!            .and. (.not. qFound)) then
!            queue = "icexq"
!            qFound = .true.
!        !    la queue d’exécution icexmemq :
!        !
!        !        ressources limites :
!        !          + temps d'exécution < 24 heures
!        !          + nombre de coeurs < 24
!        !          + mémoire < 32 Gio
!        !        jobs cibles : jobs séquentiels et de type OpenMP
!        !        nœuds cibles : 4 nœuds de l'ICEX
!
!        end if
!        if((timePerProc < 24*h .and. memPerProc < 46*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
!            .and. (.not. qFound)) then
!            queue = "iceq"
!            qFound = .true.
!        !    la queue d’exécution icemem48gbq :
!        !
!        !        ressources limites :
!        !          + temps d'exécution < 24 heures
!        !          + nombre de coeurs < 12
!        !          + mémoire < 46 Gio
!        !        jobs cibles : jobs séquentiels et de type OpenMP
!        !        nœuds cibles : 8 nœuds de 48 Gio (+ 4 nœuds de 72 Gio si libres)
!
!        end if
!        if((timePerProc < 24*h .and. memPerProc < 70*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
!            .and. (.not. qFound)) then
!            queue = "iceq"
!            qFound = .true.
!        !     la queue d’exécution icemem72gbq :
!        !
!        !        ressources limites :
!        !          + temps d'exécution < 24 heures
!        !          + nombre de coeurs < 12
!        !          + mémoire < 70 Gio
!        !        jobs cibles : jobs séquentiels et de type OpenMP necessitant plus de 46 Gio
!        !        nœuds cibles : 4 nœuds de 72 Gio
!
!        !        else if(timePerProc < 24*h .and. memPerProc < 128*gb .and. nChunks == 1 .and. nProcsTotal < 12) then
!        !            queue = "uvq"
!        !Queue uvq
!        !
!        !    ressources limites :
!        !    + temps d'exécution < 24 heures
!        !    + nombre de coeurs < 12
!        !    + mémoire < 128 Gio
!        !    jobs cibles : jobs sequentiels et de type OpenMP nécessitant beaucoup de mémoire
!        !
!        end if
!
!        !MULTICHUNK 20m-------------------------------------------------------------------
!        if((timePerProc < 20*m .and. nProcsTotal < 24) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 24*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "iceq"
!                qFound = .true.
!            end if
!
!            !    la queue d’exécution icetestq :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 20 minutes
!            !          + nombre de coeurs < 24
!            !          + mémoire par noeuds < 24 Gio
!            !        jobs cibles : tout type de jobs
!            !        nœuds cibles : 2 nœuds de 24 Gio
!
!        end if
!        if((timePerProc < 20*m .and. nProcsTotal < 36) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 32*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "icexq"
!                qFound = .true.
!            end if
!
!            !     la queue d’exécution icextestq :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 20 minutes
!            !          + nombre de coeurs < 36
!            !          + mémoire par noeuds < 32 Gio
!            !        jobs cibles : tout type de jobs
!            !        nœuds cibles : 2 nœuds de l'ICEX
!        end if
!
!
!        !MULTICHUNK 4h-------------------------------------------------------------------
!        if((timePerProc < 4*h .and. nProcsTotal < 144) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 32*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "icexq"
!                qFound = .true.
!            end if
!            !     la queue d’exécution icexparq :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 4 heures
!            !          + nombre de coeurs < 144
!            !          + mémoire par noeuds < 32 Gio
!            !        jobs cibles : jobs parallèle de type MPI
!            !        nœuds cibles : 6 nœuds de l'ICEX
!
!        end if
!        if((timePerProc < 4*h .and. nProcsTotal < 156) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 24*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "iceq"
!                qFound = .true.
!            end if
!            !    la queue d'exécution icepar156q :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 4 heures
!            !          + nombre de coeurs < 156
!            !          + mémoire par noeuds < 24 Gio
!            !        jobs cibles : jobs parallèle de type mpi
!            !        nœuds cibles : 56 noeuds de 24 Gio de mémoire
!
!        end if
!        if((timePerProc < 4*h .and. nProcsTotal < 241) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 32*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "icexq"
!                qFound = .true.
!            end if
!
!            !     la queue d’exécution np_icexpar240q :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 4 heures
!            !          + nombre de coeurs > 144
!            !          + nombre de coeurs < 241
!            !          + mémoire par noeuds < 32 Gio
!            !        jobs cibles : jobs parallèle de type mpi
!            !        contrainte : les jobs soumis dans cette queue ne s'exécutent que la nuit et le week-end
!            !        nœuds cibles : 10 nœuds de l'ICEX
!        end if
!        if((timePerProc < 4*h .and. nProcsTotal < 516) &
!            .and. (.not. qFound)) then
!            memMaxPerChunk = 24*gb
!            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
!            if(nProcsPerChunk <= nProcsMaxPerChunk) then
!                queue = "iceq"
!                qFound = .true.
!            end if
!            !    la queue d'exécution np_icepar516q :
!            !
!            !        ressources limites :
!            !          + temps d'exécution < 4 heures
!            !          + nombre de coeurs < 516
!            !          + mémoire par noeuds < 24 Gio
!            !        contrainte : les jobs soumis dans cette queue ne s'exécutent que la nuit et le week-end
!            !        jobs cibles : jobs parallèle de type mpi
!            !        nœuds cibles : 56 nœuds de 24 Gio de mémoire
!        end if
!
!        if(.not. qFound) then
!            write(*,*) "WARNING!! The queue was not founded"
!        else
!            write(*,*) "The queue was founded"
!            write(*,*) "queue = ", queue
!        end if
!
!    end function choose_queue
!

end program main_AutoTest
