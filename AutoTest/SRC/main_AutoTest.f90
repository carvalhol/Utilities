program main_AutoTest

    use systemUt_RF
    use charFunctions

    implicit none

    !USER

    integer :: nIter = 8
    logical :: weakScal = .true.
    logical :: singleProc = .false.
    integer :: memBase = 4
    character(len=200) :: execPath = "/home/carvalhol/Projects/RANDOM_FIELD/build/randomField.exe "

    !GENERAL NAMES AND PATHS
    character(len=50) :: res_folder
    character(len=50) :: dim_folder
    character(len=50) :: method_folder
    character(len=50) :: it_folder
    !character(len=200) :: basic_path
    character(len=200) :: res_path
    character(len=200) :: dim_path
    character(len=200) :: method_path
    character(len=200) :: it_path
    character(len=200) :: runAll_path
    character(len=200) :: PBS_path
    character(len=200) :: mesh_path
    character(len=200) :: gen_path
    character(len=50) :: genName = "gen_input"
    character(len=50) :: meshName = "mesh_input"

    !PBS
    character(len=50) :: pbsName
    character(len=50) :: jobName
    character(len=50) :: outName
    integer :: nProcsTotal
    integer :: memTotal
    character(len=50) :: wallTime
    character(len=5) :: queue

    !GENERATION
    integer :: Nmc = 1
    character (len=15) :: corrMod = "gaussian"
    character (len=15) :: margiFirst = "lognormal"
    double precision   :: fieldAvg = 0.0D0, fieldVar=1.0D0;
    double precision, dimension(:), allocatable :: corrL;
    integer :: nDim
    integer :: method !1 for Isotropic, 2 for Shinozuka, 3 for Randomization

    !MESH
    character (len=15) :: meshType = "unstructured"
    character (len=15) :: meshMod  = "automatic";
    double precision, dimension(:), allocatable :: xMax, xMin, xStep;

    !CONSTANTS
    integer, parameter :: ISOTROPIC = 1, &
        SHINOZUKA = 2, &
        RANDOMIZATION = 3
    integer, parameter :: nMaxProcPerNoeud = 12

    !LOCAL
    integer :: fileId = 15
    integer :: compiler = 2 !1 for gfortran and 2 for ifort
    logical :: dirExists

    integer :: runAll_Id = 17
    integer :: nProcsPerNoeud_chSz
    integer :: nNoeuds_chSz
    integer :: memPerNoeud_chSz
    integer :: nDim_chSz
    character(len=200) :: format
    !character(len=50) :: folder_name
    integer :: i, nTests, secur = 0
    character(len=5) :: methodTxt
    character(len=1) :: testTypeChar
    logical :: qFound, writeFiles, initial
    integer, parameter :: dimMax = 3
    integer, parameter :: methodMax = 3

    !FOR FILE CREATION
    character(len=200) :: results_path
    integer :: memPerNoeud
    integer :: nProcsPerNoeud
    integer :: nNoeuds
    integer :: pos

    write(6,*) "Screen is 6"

    !Master folder creation
    if(singleProc) then
        weakScal = .true.
        res_folder = "COMP"
        testTypeChar = "C"
    else if(weakScal) then
        res_folder = "WEAK"
        testTypeChar = "W"
    else
        res_folder = "STRONG"
        testTypeChar = "S"
    end if

    res_path = string_vec_join(["../", res_folder])
    call check_folder_existence(res_path, ".", compiler, dirExists)
    if(.not. dirExists) call create_folder(res_path, ".")


    dimLoop : do nDim = 1, dimMax
        write(*,*) "----------------------------------------"
        write(*,*) "nDim = ", nDim

        initial = .true.

        !runAll_file creation
        runAll_path = string_vec_join([res_path, "/runAll_",numb2String(nDim),"D.sh"])
        write(*,*) "runAll_path = ", runAll_path
        open (unit = runAll_Id , file = runAll_path, action = 'write')
        write(runAll_Id,"(A)") "#!/bin/bash"
        write(runAll_Id,"(A)") ""
        write(runAll_Id,"(A)") "clear"

        dim_folder = string_vec_join([numb2String(nDim),"D_", testTypeChar])
        dim_path = string_vec_join([res_path,"/",dim_folder])
        call check_folder_existence(dim_path, ".", compiler, dirExists)
        if(.not. dirExists) call create_folder(dim_path, ".")
        write(*,*) 'dim_path = ', trim(dim_path)

        methodLoop : do method = 1, methodMax

            if(method == ISOTROPIC .and. nDim == 1) cycle !No isotropic 1D

            if(method == ISOTROPIC) methodTxt = "ISO  "
            if(method == SHINOZUKA) methodTxt = "SHINO"
            if(method == RANDOMIZATION) methodTxt = "RANDO"

            write(*,*) ""
            write(*,*) ""
            write(*,*) "method = ", method
            write(*,*) "methodTxt = ", methodTxt
            write(*,*) ""

            !Type_of_test folder creation\
            method_folder = string_vec_join([methodTxt, "_", numb2String(nDim), "D_", testTypeChar])
            method_path = string_vec_join([dim_path,"/", method_folder])
            !basic_path = string_vec_join([res_folder,"/",numb2String(nDim),"D_it",numb2String(it),"_",methodTxt])
            call check_folder_existence(method_path, ".", compiler, dirExists)
            if(.not. dirExists) call create_folder(method_path, ".")
            write(*,*) 'method_path = ', trim(method_path)

            !Running iterations
            iterationLoop : do nTests = 1, nIter

                !Defining Number of Procs
                write(*,*) ""
                write(*,*) "iteration = ", nTests
                nProcsTotal = 2**(nTests-1)
                if(singleProc) nProcsTotal = 1
                write(*,*) "    nProcs INITIAL = ", nProcsTotal

                !Defining Memory and Number of Noeuds
                memTotal = memBase
                if(weakScal) memTotal = nProcsTotal*memBase
                if(singleProc) memTotal = 2**(nTests-1)*memBase
                nProcsPerNoeud = nProcsTotal
                if(nProcsPerNoeud > nMaxProcPerNoeud) then
                    nProcsPerNoeud = 8
                end if
                nNoeuds = nProcsTotal/nProcsPerNoeud
                memPerNoeud = ceiling(dble(memTotal)/dble(nNoeuds))

                !Defining adapted queue
                !wallTime = "23:59:00"
                !queue = "iceq"
                !queue = "icexq"
                secur = 0
                qFound = .false.
                writeFiles = .false.
                do while((.not. qFound) .and. secur < 10)
                    secur = secur + 1
                    qFound = choose_queue()
                    if(qFound) then
                        write(*,*) "qFound = ", qFound
                        writeFiles = .true.
                    else if(secur < 10) then
                        if(nNoeuds == 1) then
                            if(memPerNoeud > 127) then
                                write(*,*) "Modifying memPerNoeud "
                                write(*,*) "            OLD = ",memPerNoeud
                                memPerNoeud = 127
                                memTotal = memPerNoeud * nNoeuds
                                write(*,*) "            NEW = ",memPerNoeud
                                write(*,*) "       memTotal = ",memTotal
                            end if
                        else if(memPerNoeud > 31) then
                            write(*,*) "Modifying memPerNoeud "
                            write(*,*) "            OLD = ",memPerNoeud
                            memPerNoeud = 31
                            memTotal = memPerNoeud * nNoeuds
                            write(*,*) "            NEW = ",memPerNoeud
                            write(*,*) "       memTotal = ",memTotal
                        else if(nProcsTotal > 144) then
                            write(*,*) "Modifying nProcsTotal "
                            write(*,*) "            OLD = ",nProcsTotal
                            nProcsTotal = 128
                            nNoeuds = nProcsTotal/nProcsPerNoeud
                            memPerNoeud = ceiling(dble(memTotal)/dble(nNoeuds))

                            write(*,*) "            NEW = ",nProcsTotal
                            write(*,*) "        nNoeuds = ",nNoeuds
                            write(*,*) "    memPerNoeud = ",memPerNoeud

                        end if
                    else
                        write(*,*) "Problem findind queue for"
                        write(*,*) "    dim = ", nDim
                        write(*,*) " method = ", method
                        write(*,*) "   iter = ", nTests
                        write(*,*) "   nProcsTotal = ", nProcsTotal
                        write(*,*) "   nNoeuds     = ", nNoeuds
                        write(*,*) "   memPerNoeud = ", memPerNoeud
                        write(*,*) "nProcsPerNoeud = ", nProcsPerNoeud
                    end if
                end do

                !Defining Names
                pbsName = string_vec_join(["run", numb2String(nProcsTotal), ".pbs"])
                jobName = string_vec_join([numb2String(nDim),"D_", numb2String(nProcsTotal),"_",testTypeChar,"_",methodTxt])
                outName = string_vec_join([numb2String(nDim),"D_", numb2String(nProcsTotal),"_",testTypeChar,"_",methodTxt, ".txt"])
                write(*,*) "    pbsName = ", pbsName
                write(*,*) "    jobName = ", jobName
                write(*,*) "    outName = ", outName

                !Iteration folder creation
                it_folder = string_vec_join(["it", numb2String(nTests),"_",numb2String(nProcsTotal),"p_", methodTxt, "_", numb2String(nDim), "D_", testTypeChar])
                it_path = string_vec_join([method_path,"/", it_folder])
                !folder_name = string_vec_join([numb2String(nProcsTotal),"_", numb2String(nDim),"D_",methodTxt])
                write(*,*) "    Iteration path = ", trim(it_path)
                call check_folder_existence(it_path, ".", compiler, dirExists)
                if(.not. dirExists) call create_folder(it_path, ".")


                !GENERATION FILE
                Nmc = 1
                corrMod = "gaussian"
                margiFirst = "lognormal"
                call set_vec(corrL, [(1.0D0, i=1, nDim)])
                fieldAvg = 0.5D0
                fieldVar = 1.0D0

                !MESH FILE
                call set_vec(xMax, [(10.0D0, i=1, nDim)])
                call set_vec(xMin, [(0.0D0, i=1, nDim)])
                call set_vec(xStep, [(0.1D0, i=1, nDim)])
                meshType = "unstructured"
                meshMod = "automatic"

                !WEAK SCALLING
                if(weakScal) then
                    do i = 2, nTests
                        pos  = mod(i-2+nDim,nDim) + 1
                        xMax(pos) = 2* xMax(pos)
                    end do
                    write(*,*) "    xMax = ", xMax
                end if

                !WRITING FILES
                if(size(corrL) /= nDim .or. size(xMax) /= nDim .or. &
                    size(xMin) /= nDim  .or. size(xStep) /= nDim) then
                    write(*,*) "ERROR!!!"
                    write(*,*) "nDim = ",nDim
                    write(*,*) "corrL = ",corrL
                    write(*,*) "xMax = ",xMax
                    write(*,*) "xMin = ",xMin
                    write(*,*) "xStep = ",xStep
                    stop "Dimensions are not compatible"
                end if

                if (writeFiles) then
                    write(*,*) "-> Writing files"

                    write(*,*) "    *PBS"
                    call writePBSfile()
                    write(*,*) "    *generation"
                    call write_gen_file()
                    write(*,*) "    *mesh"
                    call write_mesh_file()
                    if(initial) then
                        write(runAll_Id,"(A)") "cd "//trim(dim_folder)
                        write(runAll_Id,"(A)") "cd "//trim(method_folder)
                        write(runAll_Id,"(A)") "cd "//trim(it_folder)
                    else
                        write(runAll_Id,"(A)") "cd ../../"//trim(method_folder)
                        !write(runAll_Id,"(A)") "cd "//trim(method_folder)
                        write(runAll_Id,"(A)") "cd "//trim(it_folder)
                    end if
                    write(runAll_Id,"(A)") "qsub "//trim(pbsName)
                    initial = .false.
                end if

            end do iterationLoop

        end do methodLoop

        write(runAll_Id,"(A)") ""
        write(runAll_Id,"(A)") "qstat -u carvalhol"
        close (runAll_Id)
        write(*,*) "-> runAll done"
        call system("chmod u+x "//trim(runAll_path))
        call system("chmod a+r "//trim(runAll_path))

    end do dimLoop

contains
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine writePBSfile()

        nDim_chSz = findCharSize(nDim)
        nProcsPerNoeud_chSz = findCharSize(nProcsPerNoeud)
        nNoeuds_chSz = findCharSize(nNoeuds)
        memPerNoeud_chSz = findCharSize(memPerNoeud)

        PBS_path = string_vec_join([it_path, "/", pbsName])

        open (unit = fileId , file = PBS_path, action = 'write')

        write(fileId,"(A)") "#!/bin/bash"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "#PBS -S /bin/bash"
        write(fileId,"(A8,A50)") "#PBS -N ", jobName
        write(fileId,"(A8,A50)") "#PBS -o ", outName
        write(fileId,"(A)") "#PBS -j oe"
        write(fileId,"(A17,A8)") "#PBS -l walltime=", wallTime
        format = string_vec_join(["(A15,A",numb2String(nNoeuds_chSz),",A7,A",numb2String(nProcsPerNoeud_chSz),", A10, A", numb2String(nProcsPerNoeud_chSz),", A5, A", numb2String(memPerNoeud_chSz),", A2  )"])
        !write(*,*) "format = ", format
        write(fileId,format) "#PBS -l select=", numb2String(nNoeuds), ":ncpus=",numb2String(nProcsPerNoeud),":mpiprocs=",numb2String(nProcsPerNoeud),":mem=", numb2String(memPerNoeud), "gb"
        !write(fileId,"(A15,A1,A7,A1, A10, A1, A5, A2, A1  )") "#PBS -l select=", numb2String(nNoeuds), ":ncpus=",numb2String(nProcsPerNoeud),":mpiprocs=",numb2String(nProcsPerNoeud),":mem=", numb2String(memPerNoeud), "gb"
        write(fileId,"(A)") "#PBS -q "//queue
        write(fileId,"(A)") "#PBS -M lucianopaludoecp@gmail.com"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "# chargement des modules"
        write(fileId,"(A)") "module load intel-compiler/14.0.0"
        write(fileId,"(A)") "module load intel-mkl/11.1.0"
        write(fileId,"(A)") "module load intel-mpi/4.0.0.028"
        write(fileId,"(A)") "module load hdf5/1.8.12"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "# On se place dans le repertoire depuis lequel le job a ete soumis"
        write(fileId,"(A)") "cd $PBS_O_WORKDIR"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "cat $PBS_NODEFILE | uniq > mpd.hosts"
        write(fileId,"(A)") "nb_nodes=`cat mpd.hosts|wc -l`"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "mpirun --rsh=ssh -n $nb_nodes -f mpd.hosts -np "//trim(numb2String(nProcsTotal))//trim(execPath)

        close(fileId)

        call system("chmod a+r "//trim(PBS_path))

    end subroutine writePBSfile

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine write_gen_file()

        gen_path = string_vec_join([it_path, "/", genName])

        open (unit = fileId , file = gen_path, action = 'write')

        write(fileId,*) "$$Nmc "
        write(fileId,*) Nmc
        write(fileId,*) "$$corrMod ", corrMod
        write(fileId,*) "$$margiFirst ", margiFirst
        write(fileId,*) "$corrL "
        write(fileId,*) corrL
        write(fileId,*) "$$fieldAvg "
        write(fileId,*) fieldAvg
        write(fileId,*) "$$fieldVar "
        write(fileId,*) fieldVar
        write(fileId,*) "$$method "
        write(fileId,*) method

        close(fileId)

        call system("chmod a+r "//trim(gen_path))

    end subroutine write_gen_file

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine write_mesh_file()

        mesh_path = string_vec_join([it_path, "/", meshName])

        open (unit = fileId , file = mesh_path, action = 'write')

        write(fileId,*) "$$meshType ", meshType
        write(fileId,*) "$$meshMod ", meshMod
        write(fileId,*) "          $Min           $Max          $Step"
        do i = 1, nDim
            write(fileId, "(3F15.5)") xMin(i), xMax(i), xStep(i)
        end do

        close(fileId)

        call system("chmod a+r "//trim(mesh_path))

    end subroutine write_mesh_file

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function choose_queue() result(qFound)

        implicit none
        integer :: icex_proc_max = 25
        integer :: icex_mem_max = 32
        integer :: ice_proc_max = 13
        double precision :: qFactor, new_qFactor
        logical :: qFound

        qFound = .false.
        qFactor = 0.0D0

!ICEX------------------------------------------------------------------
!    Queue icexq
!
!La queue de soumission icexq permet de rédiriger les jobs soumis, sur les noeuds de l'Altix ICE selon les ressources demandees vers :
!
        if(memPerNoeud < icex_mem_max .and. nProcsPerNoeud < icex_proc_max) then
            if(nNoeuds == 1 .and. nProcsTotal < 24) then
                new_qFactor = dble(nProcsTotal)/24.0D0 * dble(memPerNoeud)/dble(icex_mem_max)
!    la queue d’exécution icexmemq :
!
!        ressources limites :
!          + temps d'exécution < 24 heures
!          + nombre de coeurs < 24
!          + mémoire < 32 Gio
!        jobs cibles : jobs séquentiels et de type OpenMP
!        nœuds cibles : 4 nœuds de l'ICEX
            end if
            if(nProcsTotal < 144) then
!     la queue d’exécution icexparq :
!
!        ressources limites :
!          + temps d'exécution < 4 heures
!          + nombre de coeurs < 144
!          + mémoire par noeuds < 32 Gio
!        jobs cibles : jobs parallèle de type MPI
!        nœuds cibles : 6 nœuds de l'ICEX
                new_qFactor = dble(nProcsTotal)/144.0D0 &
                            * dble(memPerNoeud)/dble(icex_mem_max) &
                            * dble(nProcsPerNoeud)/dble(icex_proc_max) &
                            * dble(nNoeuds)/dble(nNoeuds)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "icexq"
                    wallTime = "03:59:00"
                end if
            end if
            if(nProcsTotal < 36) then
!     la queue d’exécution icextestq :
!
!        ressources limites :
!          + temps d'exécution < 20 minutes
!          + nombre de coeurs < 36
!          + mémoire par noeuds < 32 Gio
!        jobs cibles : tout type de jobs
!        nœuds cibles : 2 nœuds de l'ICEX
                new_qFactor = dble(nProcsTotal)/36.0D0 &
                            * dble(memPerNoeud)/dble(icex_mem_max) &
                            * dble(nProcsPerNoeud)/dble(icex_proc_max) &
                            * dble(nNoeuds)/dble(nNoeuds)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "icexq"
                    wallTime = "00:19:00"
                end if
            end if
            if(nProcsTotal > 144 .and. nProcsTotal < 241) then
!     la queue d’exécution np_icexpar240q :
!
!        ressources limites :
!          + temps d'exécution < 4 heures
!          + nombre de coeurs > 144
!          + nombre de coeurs < 241
!          + mémoire par noeuds < 32 Gio
!        jobs cibles : jobs parallèle de type mpi
!        contrainte : les jobs soumis dans cette queue ne s'exécutent que la nuit et le week-end
!        nœuds cibles : 10 nœuds de l'ICEX
                new_qFactor = dble(nProcsTotal)/241.0D0 &
                            * dble(memPerNoeud)/dble(icex_mem_max) &
                            * dble(nProcsPerNoeud)/dble(icex_proc_max) &
                            * dble(nNoeuds)/dble(nNoeuds)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "icexq"
                    wallTime = "03:59:00"
                end if
            end if
        end if


!ICE------------------------------------------------------------------
!
!Queue iceq
!
!La queue de soumission iceq permet de rédiriger les jobs soumis, sur les noeuds de l'Altix ICE selon les ressources demandees vers :
!
        if(nProcsPerNoeud < ice_proc_max) then
            if(nProcsTotal < 24 .and. memPerNoeud < 24) then
!    la queue d’exécution icetestq :
!
!        ressources limites :
!          + temps d'exécution < 20 minutes
!          + nombre de coeurs < 24
!          + mémoire par noeuds < 24 Gio
!        jobs cibles : tout type de jobs
!        nœuds cibles : 2 nœuds de 24 Gio
                new_qFactor = dble(nProcsTotal)/24.0D0 &
                            * dble(memPerNoeud)/24.0D0 &
                            * dble(nProcsPerNoeud)/dble(ice_proc_max) &
                            * dble(nNoeuds)/dble(24.0D0)/dble(ice_proc_max)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "iceq"
                    wallTime = "00:19:00"
                end if
            end if
            if(nNoeuds == 1 .and. nProcsTotal < 12 .and. memPerNoeud < 46) then
!    la queue d’exécution icemem48gbq :
!
!        ressources limites :
!          + temps d'exécution < 24 heures
!          + nombre de coeurs < 12
!          + mémoire < 46 Gio
!        jobs cibles : jobs séquentiels et de type OpenMP
!        nœuds cibles : 8 nœuds de 48 Gio (+ 4 nœuds de 72 Gio si libres)
                new_qFactor = dble(nProcsTotal)/12.0D0 &
                            * dble(memPerNoeud)/46.0D0 &
                            * dble(nProcsPerNoeud)/dble(ice_proc_max) &
                            * dble(nNoeuds)/dble(12.0D0)/dble(ice_proc_max)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "iceq"
                    wallTime = "23:59:00"
                end if
            end if
            if(nNoeuds == 1 .and. nProcsTotal < 12 .and. memPerNoeud < 70) then
!     la queue d’exécution icemem72gbq :
!
!        ressources limites :
!          + temps d'exécution < 24 heures
!          + nombre de coeurs < 12
!          + mémoire < 70 Gio
!        jobs cibles : jobs séquentiels et de type OpenMP necessitant plus de 46 Gio
!        nœuds cibles : 4 nœuds de 72 Gio
                new_qFactor = dble(nProcsTotal)/12.0D0 &
                            * dble(memPerNoeud)/70.0D0 &
                            * dble(nProcsPerNoeud)/dble(ice_proc_max) &
                            * dble(nNoeuds)/dble(12.0D0)/dble(ice_proc_max)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "iceq"
                    wallTime = "23:59:00"
                end if
            end if
            if(nProcsTotal < 156 .and. memPerNoeud < 24) then
!    la queue d'exécution icepar156q :
!
!        ressources limites :
!          + temps d'exécution < 4 heures
!          + nombre de coeurs < 156
!          + mémoire par noeuds < 24 Gio
!        jobs cibles : jobs parallèle de type mpi
!        nœuds cibles : 56 noeuds de 24 Gio de mémoire
                new_qFactor = dble(nProcsTotal)/156.0D0 &
                            * dble(memPerNoeud)/24.0D0 &
                            * dble(nProcsPerNoeud)/dble(ice_proc_max) &
                            * dble(nNoeuds)/dble(156.0D0)/dble(ice_proc_max)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "iceq"
                    wallTime = "03:59:00"
                end if
            end if
            if(nProcsTotal < 516 .and. memPerNoeud < 24) then
!    la queue d'exécution np_icepar516q :
!
!        ressources limites :
!          + temps d'exécution < 4 heures
!          + nombre de coeurs < 516
!          + mémoire par noeuds < 24 Gio
!        contrainte : les jobs soumis dans cette queue ne s'exécutent que la nuit et le week-end
!        jobs cibles : jobs parallèle de type mpi
!        nœuds cibles : 56 nœuds de 24 Gio de mémoire
                new_qFactor = dble(nProcsTotal)/516.0D0 &
                            * dble(memPerNoeud)/24.0D0 &
                            * dble(nProcsPerNoeud)/dble(ice_proc_max) &
                            * dble(nNoeuds)/dble(516.0D0)/dble(ice_proc_max)
                if(new_qFactor > qFactor) then
                    qFactor = new_qFactor
                    queue = "iceq"
                    wallTime = "03:59:00"
                end if
            end if
        end if

!UVQ------------------------------------------------------------------
!
!Queue uvq
!
!La queue de soumission uvq est une queue d'exécution permettant d'utiliser les processeurs du noeud UV 100  :
        if(nNoeuds == 1 .and. nProcsPerNoeud < 12 .and. nProcsTotal < 12 .and. memPerNoeud < 128) then
!    ressources limites :
!    + temps d'exécution < 24 heures
!    + nombre de coeurs < 12
!    + mémoire < 128 Gio
!    jobs cibles : jobs sequentiels et de type OpenMP nécessitant beaucoup de mémoire
            new_qFactor = dble(nProcsTotal)/12.0D0 &
                        * dble(memPerNoeud)/128.0D0 &
                        * dble(nProcsPerNoeud)/dble(12) &
                        * dble(nNoeuds)/dble(12.0D0)/dble(12.0D0)
            if(new_qFactor > qFactor) then
                qFactor = new_qFactor
                queue = "uvq"
                wallTime = "23:59:00"
            end if
        end if

        write(*,*) "Choosing queue "
        write(*,*) "qFactor = ", qFactor
        write(*,*) "queue = ", queue
        write(*,*) "wallTime = ", wallTime

        if(qFactor > 0.0D0) qFound = .true.

    end function choose_queue


    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function findCharSize(number) result(nSize)
        !INPUT
        integer, intent(in) :: number
        !LOCAL
        integer :: nSize
        integer :: comp

        comp = 9
        nSize = 1

        do while(comp < abs(number))
            comp = comp+9*(10**nSize)
            nSize = nSize + 1
        end do

        if(number<0) nSize = nSize + 1

    end function findCharSize

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

end program main_AutoTest
