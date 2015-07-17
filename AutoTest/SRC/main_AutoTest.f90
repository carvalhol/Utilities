program main_AutoTest

    use systemUt_RF
    use charFunctions

    implicit none

    !USER

    logical :: weakScal = .true.
    logical :: singleProc = .false.
    logical :: ISO_Test = .false.
    integer :: cluster = 2 !1=Igloo, 2=Oxigen
    integer :: independent = 1 !0 = false, 1 = true (other numbers will be considered as false)
    integer, parameter :: dimMin = 3, dimMax = 3
    integer :: methodMin = 1, methodMax = 3
    integer :: nRuns = 1 !How many times each iteration
    integer :: seedStart = 0
    !logical :: ignoreQ = .false.

    double precision :: overlap = 3.0
    character(len=200) :: execPath = "/home/carvalhol/Projects/RANDOM_FIELD/build/randomField.exe "

    !COMPUTATION
    integer :: memPerNTerm = 1000 !mb
    integer :: NTerm = 1000000000 !Terms
    integer :: memBase = 2 !mb
    double precision :: timePerTerm = 1.0D-7 !s
    double precision, dimension(3), parameter :: xMaxBase = [1.0D0, 1.0D0, 1.0D0]
    integer         , dimension(3) :: nIter !number of iterations in each dimension
    integer         , dimension(3) :: iterBase !initial iteration (multiply xMaxBase)
    

    !GENERAL NAMES AND PATHS
    character(len=50) :: res_folder
    character(len=50) :: dim_folder
    character(len=50) :: method_folder
    character(len=50) :: it_folder
    character(len=50) :: Lchar
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

    !GENERATION
    integer :: Nmc = 1
    character (len=15) :: corrMod = "gaussian"
    character (len=15) :: margiFirst = "lognormal"
    double precision   :: fieldAvg = 0.0D0, fieldVar=1.0D0;
    double precision, dimension(:), allocatable :: corrL;
    integer :: nDim
    integer :: method !1 for Isotropic, 2 for Shinozuka, 3 for Randomization
    double precision :: corrLBase = 1.0D0
    double precision :: xStepBase = 0.1D0

    !MESH
    character (len=15) :: meshType = "unstructured"
    character (len=15) :: meshMod  = "automatic";
    double precision, dimension(:), allocatable :: xMax, xMin, xStep;

    !CONSTANTS
    integer, parameter :: ISOTROPIC = 1, SHINOZUKA = 2, RANDOMIZATION = 3

    !QUEUE MANAGEMENT
    integer :: proc_per_chunk_Max, mem_per_chunk_Max, n_chunk_Max
    integer :: nProcsTotal
    character(len=8) :: wallTime
    character(len=5) :: queue, errorQ = "No_q"
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
    integer :: memPerProc, memPerChunk
    integer :: timePerProc !s
    integer :: runAll_Id = 17
    integer :: nProcsPerChunk_chSz, nProcsTotal_chSz
    integer :: nChunks_chSz
    integer :: memPerChunk_chSz
    integer :: nDim_chSz
    character(len=200) :: format
    double precision :: kAdjust    = 1.0D0 !"kNStep minimum" multiplier
    double precision :: periodMult = 1.1D0 !"range" multiplier
    integer :: i, nTests, secur = 0
    character(len=5) :: methodTxt
    character(len=1) :: testTypeChar
    logical ::initial


    !FOR FILE CREATION
    character(len=200) :: results_path
    integer :: nProcsPerChunk
    integer :: nChunks
    integer :: pos

!    !Size Test
!    integer(kind=int8)  :: i8
!    integer(kind=int16) :: i16
!    integer(kind=int32) :: i32
!    integer(kind=int64) :: i64
!    integer(kind=selected_int_kind(6)) :: j6
!    integer(kind=selected_int_kind(15)):: j15


    write(6,*) "Screen is 6"

    print *, selected_int_kind(1)
    print *, selected_int_kind(2)
    print *, selected_int_kind(4)
    print *, selected_int_kind(8)
    print*, "The next is good"
    print *, selected_int_kind(16)
    print *, selected_int_kind(32)
    print *, selected_int_kind(64)
    print*, "HUGES"
    print *, huge(0_1)
    print *, huge(0_2)
    print *, huge(0_4)
    print *, huge(0_8)
    print*, "The next is cool"
    !print *, huge(0_16)

    print*, "MAX xNTotal"
    print *, huge(xNTotal)


!    print *,'Default:'
!    print *, huge(i)
!    print *,'Int8:'
!    print *, huge(i8)
!    print *,'Int16:'
!    print *, huge(i16)
!    print *,'Int32:'
!    print *, huge(i32)
!    print *,'Int64:'
!    print *, huge(i64)
!    print *,'Selected Integer Kind 6:'
!    print *, huge(j6)
!    print *,'Selected Integer Kind 15:'
!    print *, huge(j15)

    do independent = 0, 1

    if(ISO_Test) then
        methodMin = 1;
        methodMax = 1;
    end if

    !Iteration according to test type
    if(singleProc) then
        res_folder = "COMP"
        testTypeChar = "C"
        weakScal = .true.
        iterBase = [1, 1, 1] !Size of the first iteration for each dimension
        nIter = [18, 16, 13] !Number of iterations in each dimension
        if (independent == 1) then
            res_folder = "COMP-i"
        end if
    else if(weakScal) then
        res_folder = "WEAK"
        testTypeChar = "W"
        iterBase = [16, 14, 10] !MAX [18, 16, 13], Obs: with [16, 14, 11] max = 5 iterations
        if(ISO_Test) then
            iterBase = iterBase + 8 !Test for ISOGeneration
            res_folder = "WEAK_ISO"
        end if
        !res_folder = "WEAK_ISO"
        nIter = 13 ![10, 10, 10]
        if (independent == 1) then
            !iterBase = [16, 14, 9] !MAX [18, 16, 13]
            !nIter = 10 ![9, 9, 9]
            res_folder = "WEAK-i"

            if(ISO_Test) then
                !iterBase = iterBase + 8 !Test for ISOGeneration
                res_folder = "WEAK-i_ISO"
            end if
        end if
    else
        res_folder = "STRONG"
        testTypeChar = "S"
        iterBase = [17, 15, 12]
        nIter = 8 ![10, 10, 10]
        if (independent == 1) then
            res_folder = "STRONG-i"
        end if
    end if


    maxProcReq = maxval(2**(nIter-1))

    if (cluster==2) then
        queue = "NOT" !Don't apply
        proc_per_chunk_Max = 24
        mem_per_chunk_Max = 64000 !Don't apply
        n_chunk_Max = 2106
        wallTime = "10:00:00"
    else
        if(singleProc) then
            queue = "uvq"
            proc_per_chunk_Max = 1
            mem_per_chunk_Max = 128000
            n_chunk_Max = 1
            wallTime = "20:00:00"
        else if (maxProcReq < 385) then
            queue = "icexq"
            proc_per_chunk_Max = 24
            mem_per_chunk_Max = 32000
            n_chunk_Max = 16
            wallTime = "04:00:00"
        else
            queue = "iceq"
            proc_per_chunk_Max = 12
            mem_per_chunk_Max = 24000
            n_chunk_Max = 56
            wallTime = "04:00:00"
        end if
    end if

    res_path = string_vec_join(["../", res_folder])
    call check_folder_existence(res_path, ".", compiler, dirExists)
    if(.not. dirExists) call create_folder(res_path, ".")

    write(*,*)

    write(*,*) "weakScal    = ", weakScal
    write(*,*) "singleProc  = ", singleProc
    write(*,*) "execPath    = ", execPath
    write(*,*) "dimMin      = ", dimMin, " dimMax = ", dimMax
    write(*,*) "methodMin   = ", methodMin, " methodMax = ", methodMax
    write(*,*) "seedStart   = ", seedStart
    write(*,*) "nRuns       = ", nRuns
    write(*,*) "independent = ", independent
    write(*,*) "overlap     = ", overlap
    write(*,*) "res_folder   = ", res_folder
    write(*,*) "testTypeChar = ", testTypeChar
    write(*,*) "iterBase     = ", iterBase
    write(*,*) "nIter        = ", nIter

    dimLoop : do nDim = dimMin, dimMax
        write(*,*) "----------------------------------------"
        write(*,*) "nDim = ", nDim

        initial = .true.

        !runAll file creation
        runAll_path = string_vec_join([res_path, "/runAll_",numb2String(nDim),"D.sh"])
        write(*,*) "runAll_path = ", runAll_path
        open (unit = runAll_Id , file = runAll_path, action = 'write')
        write(runAll_Id,"(A)") "#!/bin/bash"
        write(runAll_Id,"(A)") ""
        write(runAll_Id,"(A)") "clear"
        write(runAll_Id,"(A)")
        write(runAll_Id,"(A)")
        write(runAll_Id,"(A)") "for i in {1.."//trim(numb2String(nRuns))//"}"
        write(runAll_Id,"(A)") "do"
        write(runAll_Id,"(A)") '   echo "Running $i"'

        !Dimension folder creation
        dim_folder = string_vec_join([numb2String(nDim),"D_", testTypeChar])
        dim_path = string_vec_join([res_path,"/",dim_folder])
        call check_folder_existence(dim_path, ".", compiler, dirExists)
        if(.not. dirExists) call create_folder(dim_path, ".")
        write(*,*) 'dim_path = ', trim(dim_path)

        methodLoop : do method = methodMin, methodMax

            if(method == ISOTROPIC .and. nDim == 1) cycle !No isotropic 1D

            if(method == ISOTROPIC) methodTxt = "ISO  "
            if(method == SHINOZUKA) methodTxt = "SHINO"
            if(method == RANDOMIZATION) methodTxt = "RANDO"
            if(method == ISOTROPIC .and. independent) methodTxt = "ISO-i  "
            if(method == SHINOZUKA .and. independent) methodTxt = "SHINO-i"
            if(method == RANDOMIZATION .and. independent) methodTxt = "RANDO-i"


            write(*,*) ""
            write(*,*) ""
            write(*,*) "method = ", method
            write(*,*) "methodTxt = ", methodTxt
            write(*,*) ""

            !Method folder creation
            method_folder = string_vec_join([methodTxt, "_", numb2String(nDim), "D_", testTypeChar])
            method_path = string_vec_join([dim_path,"/", method_folder])
            call check_folder_existence(method_path, ".", compiler, dirExists)
            if(.not. dirExists) call create_folder(method_path, ".")
            write(*,*) 'method_path = ', trim(method_path)

            !Creating iterations
            iterationLoop : do nTests = 1, nIter(nDim)

                write(*,*) "Iteration = ",nTests

                !GENERATION FILE
                Nmc = 1
                corrMod = "gaussian"
                margiFirst = "lognormal"
                call set_vec(corrL, [(corrLBase, i=1, nDim)])
                fieldAvg = 0.5D0
                fieldVar = 1.0D0

                !MESH FILE
                call set_vec(xMax, [(1.0D0, i=1, nDim)]) !Starts xMax in 1
                call set_vec(xMin, [(0.0D0, i=1, nDim)])  !Starts xMin in 0
                call set_vec(xStep, [(xStepBase, i=1, nDim)])
                meshType = "unstructured"
                meshMod = "automatic"

                !xMax MODIFICATION
                write(*,*) "xMax BEFORE = ", xMax
                if(weakScal) then
                    xMax(:) = xMax(:) * 2**(dble((nTests - 1) + iterBase(nDim)-1)/dble(nDim))
                else
                    xMax(:) = xMax(:) * 2**(dble(iterBase(nDim)-1)/dble(nDim))
                end if

                write(*,*) "xMax AFTER = ", xMax

                !-----Exigences-------------------------------------
                !NUMBER OF TERMS
                xNTotal = product(1+(xMax-xMin)/xStep)
                kNTotal = product(kAdjust*(periodMult*(xMax-xMin)/corrL + 1))

                !REQUIREMENTS
                !if(independent == 1) then
                    !memTotal = nint(dble(memPerNTerm)*2.0D0 *(dble(kNTotal)+dble(xNTotal))/dble(NTerm))
                !else
                !    memTotal = nint(dble(memPerNTerm)*(dble(kNTotal)*(dble(xNTotal))/dble(NTerm)))
                !end if
                !timeTotal = nint(dble(xNTotal)*dble(timePerTerm)*dble(kNTotal))

                !if(memTotal < 10 .and. memTotal > 0) memTotal = 10
                !if(timeTotal < 20*60)   timeTotal = 20*60
                !if(timeTotal > 24*3600) timeTotal = 24*3600

                !NUMBER OF PROCS
                nProcsTotal = 2**(nTests-1)
                if(singleProc) nProcsTotal = 1

!                !MEMORY
!                memPerProc  = ceiling(dble(memTotal)/dble(nProcsTotal))
!                if(memPerProc < 128)  memPerProc = 128

                !PRINTING
                write(*,*) "----------------------------------------------------"
                write(*,*) "Iteration ", nTests
                write(*,*) "    Inputs "
                write(*,*) "    |xMax         = ", xMax
                write(*,*) "    |xMin         = ", xMin
                write(*,*) "    |xNTotal      = ", xNTotal
                write(*,*) "    |kNTotal      = ", kNTotal
                !write(*,*) "    |memTotal     = ", memTotal
                !write(*,*) "    |timeTotal(s) = ", timeTotal
                write(*,*) "    |nProcsTotal  = ", nProcsTotal

                !CLUSTER LIMITATION
                !if(memTotal < 0)  then
                !    write(*,*) "memTotal < 0, folder will not be writen"
                !    cycle
                !end if

                !INTEGER LIMITATION
                if(xNTotal < 1 .or. kNTotal<1) then
                    write(*,*) "xNTotal or kNTotal < 1, folder will not be writen"
                    cycle
                end if

                !EXIGENCES
                nChunks = ceiling(dble(nProcsTotal)/dble(proc_per_chunk_Max))
                if(nChunks > n_chunk_Max) then
                    write(*,*) "You asked for too many chunks"
                    write(*,*) "nChunks     = ", nChunks
                    write(*,*) "n_chunk_Max = ", n_chunk_Max
                    cycle
                end if

!                memPerChunk = memPerProc * nProcsTotal * 10;
                memPerChunk = ceiling(dble(mem_per_chunk_Max)*dble(nProcsTotal)/dble(proc_per_chunk_Max))
                if(nChunks > 1) memPerChunk = mem_per_chunk_Max !proc_per_chunk_Max * memPerProc
                if(memPerChunk > mem_per_chunk_Max) then
                    write(*,*) "You asked for too much memory"
                    write(*,*) "memPerChunk       = ", memPerChunk
                    write(*,*) "mem_per_chunk_Max = ", mem_per_chunk_Max
                    cycle
                end if

                nProcsPerChunk = nProcsTotal;
                if(nChunks > 1) nProcsPerChunk = proc_per_chunk_Max

                if(nChunks < 1)     nChunks = 1
                if(memPerChunk < 1) memPerChunk = 1

                write(*,*) "    Requirements "
                !write(*,*) "    |timePerProc    = ", timePerProc
                write(*,*) "    |nProcsTotal    = ", nProcsTotal
                write(*,*) "    |memPerChunk    = ", memPerChunk
                write(*,*) "    |nChunks        = ", nChunks
                write(*,*) "    |nProcsPerChunk = ", nProcsPerChunk
                write(*,*) "    |wallTime       = ", wallTime



                !Defining Names
                pbsName = string_vec_join(["run", numb2String(nProcsTotal), ".pbs"])
                if (cluster==2) pbsName = string_vec_join(["run", numb2String(nProcsTotal), ".slurm"])
                if(singleProc)then
                    jobName = string_vec_join([numb2String(nDim),"D_i", numb2String(nTests),"_",testTypeChar,"_",methodTxt])
                    outName = string_vec_join(["out_",numb2String(nDim),"D_i", numb2String(nTests),"_",testTypeChar,"_",methodTxt, ".txt"])
                else
                    jobName = string_vec_join([numb2String(nDim),"D_", numb2String(nProcsTotal),"_",testTypeChar,"_",methodTxt])
                    outName = string_vec_join(["out_",numb2String(nDim),"D_", numb2String(nProcsTotal),"_",testTypeChar,"_",methodTxt, ".txt"])
                end if
                write(*,*) "    pbsName = ", pbsName
                write(*,*) "    jobName = ", jobName
                write(*,*) "    outName = ", outName

!                Lchar = "L"
!                do i = 1, nDim
!                    Lchar = string_vec_join([Lchar, "-", numb2String(nint(xMax(i)/xMaxBase(nDim)))])
!                end do
                Lchar = string_vec_join(["L-", numb2String(nTests)])

                !Iteration folder creation
                it_folder = string_vec_join([numb2String(nProcsTotal),"p_",Lchar,"_", methodTxt, "_", numb2String(nDim), "D_", testTypeChar])
                it_path = string_vec_join([method_path,"/", it_folder])
                !folder_name = string_vec_join([numb2String(nProcsTotal),"_", numb2String(nDim),"D_",methodTxt])
                write(*,*) "    Iteration path = ", trim(it_path)
                call check_folder_existence(it_path, ".", compiler, dirExists)
                if(.not. dirExists) call create_folder(it_path, ".")

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

                write(*,*) "-> Writing files"

                if (cluster==2) then
                    write(*,*) "    *SLURM"
                    call writeSlurmfile()
                else
                    write(*,*) "    *PBS"
                    call writePBSfile()
                end if

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
                if (cluster==2) then
                    write(runAll_Id,"(A)") "sbatch "//trim(pbsName)
                else
                    write(runAll_Id,"(A)") "qsub "//trim(pbsName)
                end if
                initial = .false.

            end do iterationLoop

        end do methodLoop
        write(runAll_Id,"(A)") "cd ../../../"
        write(runAll_Id,"(A)") "sleep 1"
        write(runAll_Id,"(A)") "done"
        write(runAll_Id,"(A)") ""
        write(runAll_Id,"(A)") "qstat -u carvalhol"
        close (runAll_Id)
        write(*,*) "-> runAll done"
        call system("chmod u+x "//trim(runAll_path))
        call system("chmod a+r "//trim(runAll_path))

    end do dimLoop

    end do

contains
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine writePBSfile()

        nDim_chSz = findCharSize(nDim)
        nProcsPerChunk_chSz = findCharSize(nProcsPerChunk)
        nChunks_chSz = findCharSize(nChunks)
        memPerChunk_chSz = findCharSize(memPerChunk)

        PBS_path = string_vec_join([it_path, "/", pbsName])

        open (unit = fileId , file = PBS_path, action = 'write')

        write(fileId,"(A)") "#!/bin/bash"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "#PBS -S /bin/bash"
        write(fileId,"(A8,A50)") "#PBS -N ", jobName
        write(fileId,"(A8,A50)") "#PBS -o ", outName
        write(fileId,"(A)") "#PBS -j oe"
        write(fileId,"(A17,A8)") "#PBS -l walltime=", wallTime
        format = string_vec_join(["(A15,A",numb2String(nChunks_chSz),",A7,A",numb2String(nProcsPerChunk_chSz),", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", numb2String(memPerChunk_chSz),", A2  )"])
        !write(*,*) "format = ", format
        write(fileId,format) "#PBS -l select=", numb2String(nChunks), ":ncpus=",numb2String(nProcsPerChunk),":mpiprocs=",numb2String(nProcsPerChunk),":mem=", numb2String(memPerChunk), "mb"
        !write(fileId,"(A15,A1,A7,A1, A10, A1, A5, A2, A1  )") "#PBS -l select=", numb2String(nChunks), ":ncpus=",numb2String(nProcsPerChunk),":mpiprocs=",numb2String(nProcsPerChunk),":mem=", numb2String(memPerChunk), "gb"
        write(fileId,"(A)") "#PBS -q "//queue
        write(fileId,"(A)") "#PBS -M lucianopaludoecp@gmail.com"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "# chargement des modules"
        !write(fileId,"(A)") "module load intel-compiler/14.0.0"
        !write(fileId,"(A)") "module load intel-mkl/11.1.0"
        !write(fileId,"(A)") "module load intel-mpi/4.0.0.028"
        !write(fileId,"(A)") "module load hdf5/1.8.12"
        write(fileId,"(A)") "module load intel-compiler/15.0.1"
        write(fileId,"(A)") "module load intel-mkl/11.2.1"
        write(fileId,"(A)") "module load intel-mpi/5.0.2"
        write(fileId,"(A)") "module load hdf5/1.8.12"

        write(fileId,"(A)") ""
        write(fileId,"(A)") "# On se place dans le repertoire depuis lequel le job a ete soumis"
        write(fileId,"(A)") "cd $PBS_O_WORKDIR"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "cat $PBS_NODEFILE | uniq > mpd.hosts"
        write(fileId,"(A)") "nb_nodes=`cat mpd.hosts|wc -l`"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "mpirun -np "//trim(numb2String(nProcsTotal))//" "//trim(execPath)
        !write(fileId,"(A)") "mpirun --rsh=ssh -n $nb_nodes -f mpd.hosts -np "//trim(numb2String(nProcsTotal))//" "//trim(execPath)

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

        write(fileId,*) "$$nDim ", nDim
        write(fileId,*) "$$Nmc ", Nmc
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
        write(fileId,*) "$$seedStart"
        write(fileId,*) seedStart
        write(fileId,*) "$$independent"
        write(fileId,*) independent
        write(fileId,*) "$$overlap"
        write(fileId,*) overlap

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

        write(fileId,*) "$$nDim ", nDim
        write(fileId,*) "$$meshType ", meshType
        write(fileId,*) "$$meshMod ", meshMod
        write(fileId,*) "          $Min            $Max           $Step"
        do i = 1, nDim
            write(fileId, "(3(F15.5, A))") xMin(i), " ", xMax(i), " ", xStep(i), " "
        end do

        close(fileId)

        call system("chmod a+r "//trim(mesh_path))

    end subroutine write_mesh_file

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine writeSlurmfile()

        nDim_chSz = findCharSize(nDim)
        nProcsPerChunk_chSz = findCharSize(nProcsPerChunk)
        nChunks_chSz = findCharSize(nChunks)
        memPerChunk_chSz = findCharSize(memPerChunk)
        nProcsTotal_chSz = findCharSize(nProcsTotal)

        PBS_path = string_vec_join([it_path, "/", pbsName])

        open (unit = fileId , file = PBS_path, action = 'write')

!#!/bin/bash
!
!#SBATCH -J Mesh_SEM
!#SBATCH --nodes=1
!#SBATCH --ntasks=1
!#SBATCH --ntasks-per-node=1
!#SBATCH --threads-per-core=1
!#SBATCH --time=00:01:00
!#SBATCH --output output.txt
!#SBATCH --mail-type=ALL
!#SBATCH --mail-user=lucio.a.c@gmail.com
!
!module purge
!module load intel/15.0.0.090
!module load bullxmpi/1.2.8.4
!module load hdf5/1.8.14
!srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS /panfs/panasas/cnt0025/mss7417/abreul/SEM/build/MESH/mesher<mesh.input
        !,",A7,A",numb2String(nProcsPerChunk_chSz),", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", numb2String(memPerChunk_chSz),", A2  )"])

        write(fileId,"(A)") "#!/bin/bash"
        write(fileId,"(A)") ""
        write(fileId,"(A11,A50)") "#SBATCH -J ", jobName
        format = string_vec_join(["(A16,A",numb2String(nChunks_chSz),")"])
        write(fileId,format) "#SBATCH --nodes=", numb2String(nChunks)
        format = string_vec_join(["(A17,A",numb2String(nProcsTotal_chSz),")"])
        write(fileId,format) "#SBATCH --ntasks=", trim(numb2String(nProcsTotal))
        format = string_vec_join(["(A26,A",numb2String(nProcsPerChunk_chSz),")"])
        write(fileId,format) "#SBATCH --ntasks-per-node=", numb2String(nProcsPerChunk)
        write(fileId,"(A)") "#SBATCH --threads-per-core=1"
        write(fileId,"(A15,A8)") "#SBATCH --time=", wallTime
        write(fileId,"(A17,A)") "#SBATCH --output ", trim(outName)
        write(fileId,"(A)") "#SBATCH --mail-type=ALL"
        write(fileId,"(A)") "#SBATCH --mail-user=lucio.a.c@gmail.com"
        write(fileId,"(A)") ""
        !format = string_vec_join(["(A15,A",numb2String(nChunks_chSz),",A7,A",numb2String(nProcsPerChunk_chSz),", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", numb2String(memPerChunk_chSz),", A2  )"])

        write(fileId,"(A)") "module load intel-compiler/15.0.0.090"
        !write(fileId,"(A)") "module load intel-mkl/11.2.1"
        write(fileId,"(A)") "module load bullxmpi/1.2.8.4"
        write(fileId,"(A)") "module load hdf5/1.8.14"
        write(fileId,"(A)") "srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS "//trim(execPath)
        !write(fileId,"(A)") "mpirun --rsh=ssh -n $nb_nodes -f mpd.hosts -np "//trim(numb2String(nProcsTotal))//" "//trim(execPath)

        close(fileId)

        call system("chmod a+r "//trim(PBS_path))

    end subroutine writeSlurmfile

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function find_WallTime(timeInSec) result(wallTimeText)

        integer, intent(in) :: timeInSec
        character(len=8)    :: wallTimeText

        integer :: restantTime
        integer :: compTime
        integer :: div

        wallTimeText = "--------"

        if(timeInSec > 24*3600) then
            wallTimeText = "--------"
        else
            restantTime = timeInSec

            !Hours
            div = 3600
            compTime = restantTime/div
            if(findCharSize(compTime) /= 2)then
                wallTimeText(1:1) = "0"
                wallTimeText(2:2) = numb2String(compTime)
            else
                wallTimeText(1:2) = numb2String(compTime)
            end if
            restantTime = timeInSec - compTime*div

            !Minutes
            div = 60
            compTime = restantTime/div
            wallTimeText(3:3) = ":"
            if(findCharSize(compTime) /= 2)then
                wallTimeText(4:4) = "0"
                wallTimeText(5:5) = numb2String(compTime)
            else
                wallTimeText(4:5) = numb2String(compTime)
            end if
            restantTime = timeInSec - compTime*div

            !Seconds
            div = 1
            compTime = restantTime/div
            wallTimeText(6:6) = ":"
            if(findCharSize(compTime) /= 2)then
                wallTimeText(7:7) = "0"
                wallTimeText(8:8) = numb2String(compTime)
            else
                wallTimeText(7:8) = numb2String(compTime)
            end if

            wallTimeText(8:8) = "0"
        end if


    end function find_WallTime

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function choose_queue() result(qFound)

        implicit none
        integer :: m = 60, h = 3600, gb = 1024
        logical :: qFound
        integer :: memMaxPerChunk
        integer :: nProcsMaxPerChunk

        qFound = .false.
        queue  = errorQ


    !MONOCHUNK-----------------------------------------------------------------------------
        !START TEST
        if((timePerProc < 24*h .and. memPerProc < 100*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
           .and. (.not. qFound)) then
            queue = "uvq"
            qFound = .true.
        !Queue uvq
        !
        !    ressources limites :
        !    + temps d'exécution < 24 heures
        !    + nombre de coeurs < 12
        !    + mémoire < 128 Gio
        !    jobs cibles : jobs sequentiels et de type OpenMP nécessitant beaucoup de mémoire
        !
        end if
        !END TEST
        if((timePerProc < 24*h .and. memPerProc < 32*gb .and. nChunks == 1 .and. nProcsTotal < 24) &
           .and. (.not. qFound)) then
            queue = "icexq"
            qFound = .true.
        !    la queue d’exécution icexmemq :
        !
        !        ressources limites :
        !          + temps d'exécution < 24 heures
        !          + nombre de coeurs < 24
        !          + mémoire < 32 Gio
        !        jobs cibles : jobs séquentiels et de type OpenMP
        !        nœuds cibles : 4 nœuds de l'ICEX

        end if
        if((timePerProc < 24*h .and. memPerProc < 46*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
           .and. (.not. qFound)) then
            queue = "iceq"
            qFound = .true.
        !    la queue d’exécution icemem48gbq :
        !
        !        ressources limites :
        !          + temps d'exécution < 24 heures
        !          + nombre de coeurs < 12
        !          + mémoire < 46 Gio
        !        jobs cibles : jobs séquentiels et de type OpenMP
        !        nœuds cibles : 8 nœuds de 48 Gio (+ 4 nœuds de 72 Gio si libres)

        end if
        if((timePerProc < 24*h .and. memPerProc < 70*gb .and. nChunks == 1 .and. nProcsTotal < 12) &
           .and. (.not. qFound)) then
            queue = "iceq"
            qFound = .true.
        !     la queue d’exécution icemem72gbq :
        !
        !        ressources limites :
        !          + temps d'exécution < 24 heures
        !          + nombre de coeurs < 12
        !          + mémoire < 70 Gio
        !        jobs cibles : jobs séquentiels et de type OpenMP necessitant plus de 46 Gio
        !        nœuds cibles : 4 nœuds de 72 Gio

!        else if(timePerProc < 24*h .and. memPerProc < 128*gb .and. nChunks == 1 .and. nProcsTotal < 12) then
!            queue = "uvq"
        !Queue uvq
        !
        !    ressources limites :
        !    + temps d'exécution < 24 heures
        !    + nombre de coeurs < 12
        !    + mémoire < 128 Gio
        !    jobs cibles : jobs sequentiels et de type OpenMP nécessitant beaucoup de mémoire
        !
        end if

    !MULTICHUNK 20m-------------------------------------------------------------------
        if((timePerProc < 20*m .and. nProcsTotal < 24) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 24*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "iceq"
                qFound = .true.
            end if

            !    la queue d’exécution icetestq :
            !
            !        ressources limites :
            !          + temps d'exécution < 20 minutes
            !          + nombre de coeurs < 24
            !          + mémoire par noeuds < 24 Gio
            !        jobs cibles : tout type de jobs
            !        nœuds cibles : 2 nœuds de 24 Gio

        end if
        if((timePerProc < 20*m .and. nProcsTotal < 36) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 32*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "icexq"
                qFound = .true.
            end if

            !     la queue d’exécution icextestq :
            !
            !        ressources limites :
            !          + temps d'exécution < 20 minutes
            !          + nombre de coeurs < 36
            !          + mémoire par noeuds < 32 Gio
            !        jobs cibles : tout type de jobs
            !        nœuds cibles : 2 nœuds de l'ICEX
        end if


    !MULTICHUNK 4h-------------------------------------------------------------------
        if((timePerProc < 4*h .and. nProcsTotal < 144) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 32*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "icexq"
                qFound = .true.
            end if
            !     la queue d’exécution icexparq :
            !
            !        ressources limites :
            !          + temps d'exécution < 4 heures
            !          + nombre de coeurs < 144
            !          + mémoire par noeuds < 32 Gio
            !        jobs cibles : jobs parallèle de type MPI
            !        nœuds cibles : 6 nœuds de l'ICEX

        end if
        if((timePerProc < 4*h .and. nProcsTotal < 156) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 24*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "iceq"
                qFound = .true.
            end if
            !    la queue d'exécution icepar156q :
            !
            !        ressources limites :
            !          + temps d'exécution < 4 heures
            !          + nombre de coeurs < 156
            !          + mémoire par noeuds < 24 Gio
            !        jobs cibles : jobs parallèle de type mpi
            !        nœuds cibles : 56 noeuds de 24 Gio de mémoire

        end if
        if((timePerProc < 4*h .and. nProcsTotal < 241) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 32*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "icexq"
                qFound = .true.
            end if

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
        end if
        if((timePerProc < 4*h .and. nProcsTotal < 516) &
           .and. (.not. qFound)) then
            memMaxPerChunk = 24*gb
            nProcsMaxPerChunk = memMaxPerChunk / memPerProc
            if(nProcsPerChunk <= nProcsMaxPerChunk) then
                queue = "iceq"
                qFound = .true.
            end if
            !    la queue d'exécution np_icepar516q :
            !
            !        ressources limites :
            !          + temps d'exécution < 4 heures
            !          + nombre de coeurs < 516
            !          + mémoire par noeuds < 24 Gio
            !        contrainte : les jobs soumis dans cette queue ne s'exécutent que la nuit et le week-end
            !        jobs cibles : jobs parallèle de type mpi
            !        nœuds cibles : 56 nœuds de 24 Gio de mémoire
        end if

        if(.not. qFound) then
            write(*,*) "WARNING!! The queue was not founded"
        else
            write(*,*) "The queue was founded"
            write(*,*) "queue = ", queue
        end if

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
