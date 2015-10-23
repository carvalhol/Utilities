module fileManag_Auto

    use charFunctions
    use constants_Auto

    implicit none

contains

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine makeCase(nDim, Nmc, corrMod, margiFirst, corrL, fieldAvg, fieldVar, method, &
                        seedStart, independent, overlap, &
                        xMinGlob, xMaxGlob, pointsPerCorrL, &
                        nProcsTotal, nProcsPerChunk, nChunks, &
                        memPerChunk, queue, wallTime, folderPath, runPath)
        implicit none
        !INPUT
        integer, intent(in) :: nDim, Nmc, corrMod, margiFirst, method, seedStart, independent
        double precision, dimension(:), intent(in) :: corrL, overlap
        double precision, dimension(:), intent(in) :: xMinGlob, xMaxGlob
        integer, dimension(:), intent(in) :: pointsPerCorrL
        double precision, intent(in) :: fieldAvg, fieldVar
        integer, intent(in) :: nProcsTotal, nProcsPerChunk, nChunks, memPerChunk
        character(len=*), intent(in) :: wallTime
        character(len=*), intent(in) :: queue
        character(len=tSize) :: folderPath
        !OUTPUT
        character(len=*), intent(out), optional :: runPath
        !LOCAL
        character(len=tSize) :: PBS_path
        character(len=tSize) :: gen_path
        character(len=tSize) :: mesh_path
        integer :: i

        write(*,*) "Making case on: ", folderPath

        PBS_path  = string_join_many(folderPath,"/","run.pbs")
        gen_path  = string_join_many(folderPath,"/","gen_input")
        mesh_path = string_join_many(folderPath,"/","mesh_input")

        call write_mesh_file(nDim, xMinGlob, xMaxGlob, pointsPerCorrL, mesh_path)

        call write_gen_file(nDim, Nmc, corrMod, margiFirst, corrL, fieldAvg, fieldVar, method, &
                            seedStart, independent, overlap, gen_path)

        call writePBSfile(nDim, nProcsTotal, nProcsPerChunk, nChunks, &
                          memPerChunk, wallTime, queue, PBS_path)

        if(present(runPath)) runPath = PBS_path

    end subroutine makeCase

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine write_mesh_file(nDim, xMinGlob, xMaxGlob, pointsPerCorrL, mesh_path)

        implicit none
        !INPUT
        integer, intent(in) :: nDim
        double precision, dimension(:), intent(in) :: xMinGlob, xMaxGlob
        integer, dimension(:), intent(in) :: pointsPerCorrL
        character(len=*), intent(in) :: mesh_path
        !LOCAL
        integer :: i
        integer :: fileId

        fileID = 18

        open (unit = fileId , file = mesh_path, action = 'write')

        write(fileId,*) "$$nDim ", nDim
        write(fileId,*) "$$meshMod automatic"
        write(fileId,*) "          $Min            $Max           $pointsPerCorrL"
        do i = 1, nDim
            write(fileId, "(2(F15.5, A), (I15))") xMinGlob(i), " ", xMaxGlob(i), " ", pointsPerCorrL(i)
        end do

        close(fileId)

        call system("chmod a+r "//trim(mesh_path))

    end subroutine write_mesh_file

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine write_gen_file(nDim, Nmc, corrMod, margiFirst, corrL, fieldAvg, fieldVar, method, &
                              seedStart, independent, overlap, gen_path)

        implicit none
        !INPUT
        integer, intent(in) :: nDim, Nmc, corrMod, margiFirst, method, seedStart, independent
        double precision, dimension(:), intent(in) :: corrL, overlap
        double precision, intent(in) :: fieldAvg, fieldVar
        character(len=*), intent(in) :: gen_path
        !LOCAL
        integer :: fileId

        fileID = 18

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
        write(fileId,*) "$overlap"
        write(fileId,*) overlap

        close(fileId)

        call system("chmod a+r "//trim(gen_path))

    end subroutine write_gen_file

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    subroutine writePBSfile(nDim, nProcsTotal, nProcsPerChunk, nChunks, memPerChunk, wallTime, queue, PBS_path)

        implicit none
        !INPUT
        integer, intent(in) :: nDim, nProcsTotal, nProcsPerChunk, nChunks, memPerChunk
        character(len=8), intent(in) :: wallTime
        character(len=200) :: PBS_path
        character(len=*), intent(in) :: queue

        !LOCAL
        integer :: nProcsPerChunk_chSz, nProcsTotal_chSz
        integer :: nChunks_chSz
        integer :: memPerChunk_chSz
        integer :: nDim_chSz
        integer :: fileId
        character(len=200) :: format
        character(len=50) :: jobName
        character(len=50) :: outName
        integer :: i

        fileID = 28
        jobName = "jobRFGeneric"
        outName = "outRFGeneric"
        nDim_chSz = findCharSize(nDim)
        nProcsPerChunk_chSz = findCharSize(nProcsPerChunk)
        nChunks_chSz = findCharSize(nChunks)
        memPerChunk_chSz = findCharSize(memPerChunk)

        open (unit = fileId , file = trim(adjustL(PBS_path)), action = 'write')

        write(fileId,"(A)") "#!/bin/bash"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "#PBS -S /bin/bash"
        write(fileId,"(A8,A50)") "#PBS -N ", jobName
        write(fileId,"(A8,A50)") "#PBS -o ", outName
        write(fileId,"(A)") "#PBS -j oe"
        write(fileId,"(A17,A8)") "#PBS -l walltime=", wallTime
        format = string_join_many("(A15,A",numb2String(nChunks_chSz),",A7,A",numb2String(nProcsPerChunk_chSz), &
                                   ", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", &
                                   numb2String(memPerChunk_chSz),", A2  )")
        !write(*,*) "format = ", format
        write(fileId,format) "#PBS -l select=", numb2String(nChunks), ":ncpus=",numb2String(nProcsPerChunk),&
                            ":mpiprocs=",numb2String(nProcsPerChunk),":mem=", numb2String(memPerChunk), "mb"
        !write(fileId,"(A15,A1,A7,A1, A10, A1, A5, A2, A1  )") "#PBS -l select=", numb2String(nChunks), ":ncpus=",numb2String(nProcsPerChunk),":mpiprocs=",numb2String(nProcsPerChunk),":mem=", numb2String(memPerChunk), "gb"
        write(fileId,"(A)") "#PBS -q "//queue
        write(fileId,"(A)") "#PBS -M lucianopaludoecp@gmail.com"
        write(fileId,"(A)") ""
        write(fileId,"(A)") "# chargement des modules"
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

        close(fileId)

        call system("chmod a+r "//trim(PBS_path))

    end subroutine writePBSfile


!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    subroutine writeSlurmfile()
!
!        nDim_chSz = findCharSize(nDim)
!        nProcsPerChunk_chSz = findCharSize(nProcsPerChunk)
!        nChunks_chSz = findCharSize(nChunks)
!        memPerChunk_chSz = findCharSize(memPerChunk)
!        nProcsTotal_chSz = findCharSize(nProcsTotal)
!
!        PBS_path = string_join_many(it_path, "/", pbsName)
!
!        open (unit = fileId , file = PBS_path, action = 'write')
!
!    !#!/bin/bash
!    !
!    !#SBATCH -J Mesh_SEM
!    !#SBATCH --nodes=1
!    !#SBATCH --ntasks=1
!    !#SBATCH --ntasks-per-node=1
!    !#SBATCH --threads-per-core=1
!    !#SBATCH --time=00:01:00
!    !#SBATCH --output output.txt
!    !#SBATCH --mail-type=ALL
!    !#SBATCH --mail-user=lucio.a.c@gmail.com
!    !
!    !module purge
!    !module load intel/15.0.0.090
!    !module load bullxmpi/1.2.8.4
!    !module load hdf5/1.8.14
!    !srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS /panfs/panasas/cnt0025/mss7417/abreul/SEM/build/MESH/mesher<mesh.input
!            !,",A7,A",numb2String(nProcsPerChunk_chSz),", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", numb2String(memPerChunk_chSz),", A2  )"])
!
!        write(fileId,"(A)") "#!/bin/bash"
!        write(fileId,"(A)") ""
!        write(fileId,"(A11,A50)") "#SBATCH -J ", jobName
!        format = string_join_many("(A16,A",numb2String(nChunks_chSz),")")
!        write(fileId,format) "#SBATCH --nodes=", numb2String(nChunks)
!        format = string_join_many("(A17,A",numb2String(nProcsTotal_chSz),")")
!        write(fileId,format) "#SBATCH --ntasks=", trim(numb2String(nProcsTotal))
!        format = string_join_many("(A26,A",numb2String(nProcsPerChunk_chSz),")")
!        write(fileId,format) "#SBATCH --ntasks-per-node=", numb2String(nProcsPerChunk)
!        write(fileId,"(A)") "#SBATCH --threads-per-core=1"
!        write(fileId,"(A15,A8)") "#SBATCH --time=", wallTime
!        write(fileId,"(A17,A)") "#SBATCH --output ", trim(outName)
!        write(fileId,"(A)") "#SBATCH --mail-type=ALL"
!        write(fileId,"(A)") "#SBATCH --mail-user=lucio.a.c@gmail.com"
!        write(fileId,"(A)") ""
!        !format = string_join_many("(A15,A",numb2String(nChunks_chSz),",A7,A",numb2String(nProcsPerChunk_chSz),", A10, A", numb2String(nProcsPerChunk_chSz),", A5, A", numb2String(memPerChunk_chSz),", A2  )")
!
!        write(fileId,"(A)") "module load intel-compiler/15.0.0.090"
!        !write(fileId,"(A)") "module load intel-mkl/11.2.1"
!        write(fileId,"(A)") "module load bullxmpi/1.2.8.4"
!        write(fileId,"(A)") "module load hdf5/1.8.14"
!        write(fileId,"(A)") "srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS "//trim(execPath)
!        !write(fileId,"(A)") "mpirun --rsh=ssh -n $nb_nodes -f mpd.hosts -np "//trim(numb2String(nProcsTotal))//" "//trim(execPath)
!
!        close(fileId)
!
!        call system("chmod a+r "//trim(PBS_path))
!
!    end subroutine writeSlurmfile
!
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

end module fileManag_Auto
