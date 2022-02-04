module listExecutedMovesWithPossibleMoves

    type moveCoordinates
        integer, dimension(1:2) :: currentCoordinatesStart
        integer, dimension(1:2) :: currnetCoordinatesJump
        integer, dimension(1:2) :: currentCoordinatesEnd
    end type moveCoordinates

    type possibleMovesList
        integer, dimension(1:2) :: possibleCoordinatesStart
        integer, dimension(1:2) :: possibleCoordinatesJump
        integer, dimension(1:2) :: possibleCoordinatesEnd
        type(possibleMovesList), pointer::nextPossibleMove
    end type possibleMovesList

    type nodeExecutedMoves
        type(moveCoordinates)::currentMoveCoordinates
        type(nodeExecutedMoves), pointer::nextMove
        type(possibleMovesList), pointer::possibleMoves
    end type nodeExecutedMoves

    type listUnsolvablePositions
        integer, dimension(7, 7) :: matrixUnsolvable
        type(listUnsolvablePositions), pointer::nextUnsolvablePosition
    end type listUnsolvablePositions

end module listExecutedMovesWithPossibleMoves

module columnAndRowLength
      ! Wie globale Konstanten
      integer , parameter:: columnLength = 7
      integer, parameter:: rowLength = 7
      !integer:: pegCount = 32
end module columnAndRowLength


program pegSolitaire2
    use listExecutedMovesWithPossibleMoves
    use columnAndRowLength
    implicit none

    integer, dimension(columnLength , rowLength) :: matrix, matrix2


    type(nodeExecutedMoves), pointer::headExecutedMoves
    type(possibleMovesList), pointer::headPossibleMovesList
    type(moveCoordinates)::headMoveCoordinates
    type(listUnsolvablePositions), pointer::headUnsolvableMatrixList
    logical::test

    nullify(headExecutedMoves)
    nullify(headPossibleMovesList)
    nullify(headUnsolvableMatrixList)

    ! Matrix wird von .txt Datei eingelesen
    ! 'testSoli' = Matrix mit nur 11 Steinen
    ! 'testSoli2' = Volle Matrix mit 32 Steinen
    open(10,file='testSoli.txt')
    read (10,*) matrix

    !Algorithmus wird ausgefuehrt
    print *, 'Start'
    call printMatrix(matrix)
    call searchSolution(matrix, headExecutedMoves, headUnsolvableMatrixList)


    contains

    subroutine printCoordinates(coordinatesStart, coordinatesJump, coordinatesEnd)
        implicit none
        integer, dimension(1:2) :: coordinatesStart
        integer, dimension(1:2) :: coordinatesJump
        integer, dimension(1:2) :: coordinatesEnd

        ! Schreiben in der gleichen Zeile mit Variablen: schwierig deshalb so viele Aufrufe
        write(*, '(A)',advance='no') '[('
        write(*, '(i0)',advance='no') coordinatesStart(1)
        write(*, '(A)',advance='no') ','
        write(*, '(i0)',advance='no') coordinatesStart(2)
        write(*, '(A)',advance='no') '), ('
        write(*, '(i0)',advance='no') coordinatesJump(1)
        write(*, '(A)',advance='no') ','
        write(*, '(i0)',advance='no') coordinatesJump(2)
        write(*, '(A)',advance='no') '), ('
        write(*, '(i0)',advance='no') coordinatesEnd(1)
        write(*, '(A)',advance='no') ','
        write(*, '(i0)',advance='no') coordinatesEnd(2)
        write(*, '(A)',advance='no') ')], '
    end subroutine printCoordinates

    subroutine printMatrix(matrix)
        implicit none
        integer :: i,j
        integer, dimension(columnLength , rowLength), intent(inout) :: matrix

        do i = 1,columnLength
            print "(/)"
            do j = 1, rowLength
                write(*,'(1x,i0)',advance='no') matrix(j, i)
            end do
        end do
        print '(A)', ''
        print '(A)', ''
    end subroutine printMatrix

    function createArray(a, b) result(arrayResult)
        ! Erstellt ein Array mit den 2 eingegebenen Daten
        implicit none
        integer, intent(in):: a, b
        integer, dimension(1:2) :: arrayResult

        arrayResult(1)=a
        arrayResult(2)=b
    end function createArray

    subroutine insertNodeExecutedMove(headExecutedMoves, currentCoordinatesStart, currnetCoordinatesJump, currentCoordinatesEnd)
        implicit none
        type(nodeExecutedMoves), pointer::headExecutedMoves
        type(possibleMovesList), pointer::possibleMoves
        type(nodeExecutedMoves), pointer::tmp
        integer, dimension(1:2) :: currentCoordinatesStart
        integer, dimension(1:2) :: currnetCoordinatesJump
        integer, dimension(1:2) :: currentCoordinatesEnd
        type(moveCoordinates)::tmpCurrentMoveCoordinates

        allocate(tmp)
        ! daten setzen
        tmpCurrentMoveCoordinates%currentCoordinatesStart = currentCoordinatesStart
        tmpCurrentMoveCoordinates%currnetCoordinatesJump = currnetCoordinatesJump
        tmpCurrentMoveCoordinates%currentCoordinatesEnd = currentCoordinatesEnd

        tmp%currentMoveCoordinates = tmpCurrentMoveCoordinates
        call addAllPossibleMovesToList(matrix, tmp%possibleMoves)

        !Setzt Knoten als neuen Head und setzt den alten Head als Nachfolger
        !FIFO List (First in First Out)
        nullify(tmp%nextMove)
        tmp%nextMove => headExecutedMoves; headExecutedMoves => tmp
        return
    end subroutine insertNodeExecutedMove

    subroutine printListExecutedMoves(head)
        implicit none
        type(nodeExecutedMoves), pointer::tmp
        type(nodeExecutedMoves), pointer::head
        type(possibleMovesList), pointer::possibleMoves


        tmp => head ! 'Speichert' Pointer auf Head zwischen
        if(.not.associated(tmp)) then
           print *, 'no linked-list'
           return
        else
           do while(associated(tmp))
              !print *, tmp%coordinatesStart, tmp%coordinatesJump, tmp%coordinatesEnd
              print *, 'current Move:'
              call printCoordinates(tmp%currentMoveCoordinates%currentCoordinatesStart, &
              tmp%currentMoveCoordinates%currnetCoordinatesJump, &
              tmp%currentMoveCoordinates%currentCoordinatesEnd &
              )
              print '(A)', '' ! Gibt leere Zeile aus
              print *, 'possible unchecked Moves:'
              call printPossibleMovesList(tmp%possibleMoves)
              print '(A)', ''
              tmp => tmp%nextMove
           end do
           print '(A)', ''
        end if
        return
    end subroutine printListExecutedMoves

    subroutine printPossibleMovesList(head)
        implicit none
        type(possibleMovesList), pointer::tmp
        type(possibleMovesList), pointer::head

        tmp => head
        if(.not.associated(tmp)) then
           print *, 'no linked-list'
           return
        else
           print '(A)', ''
           do while(associated(tmp))
              call printCoordinates(tmp%possibleCoordinatesStart, tmp%possibleCoordinatesJump, tmp%possibleCoordinatesEnd)
              tmp => tmp%nextPossibleMove
           end do
        end if
        return
    end subroutine printPossibleMovesList

    subroutine printCurrentPath(head)
        implicit none
        type(nodeExecutedMoves), pointer::tmp
        type(nodeExecutedMoves), pointer::head
        type(possibleMovesList), pointer::possibleMoves

        tmp => head
        if(.not.associated(tmp)) then
           print *, 'no solution'
           return
        else
            print *, 'solution path'
           do while(associated(tmp))
              call printCoordinates(tmp%currentMoveCoordinates%currentCoordinatesStart, &
              tmp%currentMoveCoordinates%currnetCoordinatesJump, &
              tmp%currentMoveCoordinates%currentCoordinatesEnd &
              )
              tmp => tmp%nextMove
           end do
           print '(A)', ''
        end if
        return
    end subroutine printCurrentPath

    subroutine insertNodePossibleMove(headPossibleMovesList, iCoordinatesStart, iCoordinatesJump, iCoordinatesEnd)
        ! Fügt einen Knoten in PossibleMovesList hinzu
        ! Nach dem FIFO Prinzip wird er als neuer Head eingefügt
        implicit none
        integer, dimension(1:2) :: iCoordinatesStart
        integer, dimension(1:2) :: iCoordinatesJump
        integer, dimension(1:2) :: iCoordinatesEnd
        type(possibleMovesList), pointer::headPossibleMovesList
        type(possibleMovesList), pointer::tmp

        allocate(tmp)
        tmp%possibleCoordinatesStart = iCoordinatesStart
        tmp%possibleCoordinatesJump = iCoordinatesJump
        tmp%possibleCoordinatesEnd = iCoordinatesEnd;

        nullify(tmp%nextPossibleMove)
        tmp%nextPossibleMove => headPossibleMovesList; headPossibleMovesList => tmp
        return
    end subroutine insertNodePossibleMove

    logical function isMovePossible(matrix, startCoordinates, jumpOverCoordinates, endCoordinates) result(possible)

        implicit none
        integer, dimension(columnLength , rowLength), intent(in) :: matrix
        integer, dimension(1:2), intent(in) :: startCoordinates
        integer, dimension(1:2), intent(in) :: jumpOverCoordinates
        integer, dimension(1:2), intent(in) :: endCoordinates

        possible = .False.

        ! Testet ob Zug moeglich ist mit den Koordinaten welche die Funktion bekommt
        ! return true falls moeglich und false falls nicht
        ! Zug moeglich falls die Werte der Variablen dem Pattern 1,1,0 entsprechen
        if((matrix(startCoordinates(1), startCoordinates(2)) == 1)) then
            if((matrix(jumpOverCoordinates(1), jumpOverCoordinates(2)) == 1)) then
                if((matrix(endCoordinates(1), endCoordinates(2)) == 0)) then
                    possible = .True.
                end if
            end if
        end if

    end function isMovePossible

    subroutine addAllPossibleMovesToList(matrix, head)

        implicit none
        integer, dimension(columnLength, rowLength) :: matrix
        type(possibleMovesList), pointer::head
        integer :: i,j

        !Sucht in der gesamten Matrix nach moeglichen Zuegen
        do i = 1,columnLength
            do j = 1, rowLength

                ! Bei jeder 0 wird jeder Richtung geprüft ob ein Zug moeglich ist, wenn ja wird dieser in die Liste gepusht
                if((matrix(i,j))==0) then
                    !north
                    if((j-2)>0) then
                        if(isMovePossible(matrix, createArray(i, j-2), createArray(i, j-1), createArray(i,j))) then
                            call insertNodePossibleMove(head, createArray(i, j-2), createArray(i, j-1), createArray(i,j))
                        end if
                    end if

                    ! east
                    if((i+2)<8) then
                        if(isMovePossible(matrix, createArray(i+2, j), createArray(i+1, j), createArray(i,j))) then
                            call insertNodePossibleMove(head, createArray(i+2, j), createArray(i+1, j), createArray(i,j))
                        end if
                    end if

                    ! south
                    if((j+2)<8) then
                        if(isMovePossible(matrix, createArray(i, j+2), createArray(i, j+1), createArray(i,j))) then
                            call insertNodePossibleMove(head, createArray(i, j+2), createArray(i, j+1), createArray(i,j))
                        end if
                    end if
                    !west
                    if((i-2)>0) then
                        if(isMovePossible(matrix, createArray(i-2, j), createArray(i-1, j), createArray(i,j))) then
                            call insertNodePossibleMove(head, createArray(i-2, j), createArray(i-1, j), createArray(i,j))
                        end if
                    end if
                end if
            end do
        end do
    end subroutine addAllPossibleMovesToList

    subroutine jump(matrix, startCoordinates, jumpOverCoordinates, endCoordinates)
        implicit none
        integer, dimension(columnLength , rowLength) :: matrix
        integer, dimension(1:2), intent(in) :: startCoordinates
        integer, dimension(1:2), intent(in) :: jumpOverCoordinates
        integer, dimension(1:2), intent(in) :: endCoordinates

        ! Subroutine fuehrt auf der Matrix mit gegebenen Koordinaten den Sprung aus
        matrix(startCoordinates(1), startCoordinates(2))=0
        matrix(jumpOverCoordinates(1), jumpOverCoordinates(2))=0
        matrix(endCoordinates(1), endCoordinates(2))=1
    end subroutine jump

    subroutine jumpBack(matrix, oldStartCoordinates, oldJumpOverCoordinates, oldEndCoordinates)
        implicit none
        integer, dimension(columnLength , rowLength), intent(inout) :: matrix
        integer, dimension(1:2), intent(in) :: oldStartCoordinates
        integer, dimension(1:2), intent(in) :: oldJumpOverCoordinates
        integer, dimension(1:2), intent(in) :: oldEndCoordinates

        ! Subroutine macht auf der Matrix mit gegebenen Koordinaten den Sprung rueckgaengig
        matrix(oldStartCoordinates(1), oldStartCoordinates(2))=1
        matrix(oldJumpOverCoordinates(1), oldJumpOverCoordinates(2))=1
        matrix(oldEndCoordinates(1), oldEndCoordinates(2))=0
        end subroutine jumpBack

    subroutine deleteFirstExecutedMoveNode(headExecutedMoves)
        ! Loescht ersten Knoten in der Liste ExecutedMoves
        implicit none
        type(nodeExecutedMoves), pointer::headExecutedMoves
        type(nodeExecutedMoves), pointer::nextMove
        type(nodeExecutedMoves), pointer::tmp
        IF ( associated( headExecutedMoves ) ) THEN
           allocate(tmp)
           tmp = headExecutedMoves
           headExecutedMoves => headExecutedMoves%nextMove
           deallocate(tmp)
        END IF

        nextMove => headExecutedMoves
    end subroutine deleteFirstExecutedMoveNode

    subroutine deleteFirstPossibleMoveNode(headPossibleMovesList)
        ! Loescht ersten Knoten in der Liste PossibleMoves
        implicit none
        type(possibleMovesList), pointer::headPossibleMovesList
        type(possibleMovesList), pointer::nextPossibleMove
        type(possibleMovesList), pointer::tmp
        !print *, head%coordinatesStart, head%coordinatesJump, head%coordinatesEnd
        IF ( associated( headPossibleMovesList) ) THEN
           allocate(tmp)
           tmp = headPossibleMovesList
           headPossibleMovesList => headPossibleMovesList%nextPossibleMove
           deallocate(tmp)
        END IF

        nextPossibleMove => headPossibleMovesList
    end subroutine deleteFirstPossibleMoveNode

    subroutine searchSolution(matrix, headExecutedMoves, headUnsolvableMatrixList)
        ! Subroutine welche nach Loesungen sucht
        implicit none
        type(nodeExecutedMoves), pointer::headExecutedMoves
        integer, dimension(columnLength , rowLength) :: matrix
        type(listUnsolvablePositions), pointer::headUnsolvableMatrixList
        integer::numberOfPegs = 11 !normal 32
        logical:: unsolvable = .False.

        !Startsprung manuell ->normalerweise do loop außenrum für alle möglichen Sprünge der Startpostition
        !Mit meiner Optimierung ist es bei einen normalen Start egal
        !welcher Zug als erstes, da alle 4 sich ergebene nMatrizen kongruent zueinander
        call jump(&
            matrix,&
            (/4,3/),&
            (/3,3/),&
            (/2,3/)&
        )
            !Normales Spielsbrett erster Zug:_ (/4,2/),(/4,3/),(/4,4/)
            !testSoli erster Zug (Spielbrett mit 11 Steinen): (/4,3/),(/3,3/),(/2,3/)
        call insertNodeExecutedMove(&
            headExecutedMoves,&
            (/4,3/),&
            (/3,3/),&
            (/2,3/)&
        )
        numberOfPegs=numberOfPegs-1

        ! Solange es ausgefuehrte Zuege in der Liste gibt
        do while (associated( headExecutedMoves ))
            ! Prüft ob Zielmatrix erreicht und damit das Spiel gewonnen ist
            if(numberOfPegs == 1 .And. matrix(4, 4) == 1) then
                !Ausgabe des Zuglösungspfades
                call printCurrentPath(headExecutedMoves)
                call printMatrix(matrix)
                print *, 'win'
                exit
            end if
            !!!Optimierung
            !if(.not. (checkIfListContainsMatrix(headUnsolvableMatrixList, matrix))) then
                if(.not.(associated( headExecutedMoves%possibleMoves))) then
                        !!!Optimierung
                        !call addAllUnsolvableCongruentMatricesToList(headUnsolvableMatrixList, matrix)
                        call jumpBack(&  ! Zug rueckgaengig in Matrix
                            matrix,&
                            headExecutedMoves%currentMoveCoordinates%currentCoordinatesStart,&
                            headExecutedMoves%currentMoveCoordinates%currnetCoordinatesJump,&
                            headExecutedMoves%currentMoveCoordinates%currentCoordinatesEnd&
                        )
                        call deleteFirstExecutedMoveNode(headExecutedMoves)
                        call deleteFirstPossibleMoveNode(headExecutedMoves%possibleMoves)
                        numberOfPegs=numberOfPegs+1
                else
                        call jump(&  ! Zug in Matrix
                            matrix,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesStart,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesJump,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesEnd&
                        )

                        call insertNodeExecutedMove(&
                            headExecutedMoves,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesStart,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesJump,&
                            headExecutedMoves%possibleMoves%possibleCoordinatesEnd&
                        )
                        numberOfPegs=numberOfPegs-1
                end if
            !!!Optimierung
            !else
            !    print *, 'currentMatrixTest:'
           !     call printMatrix(matrix)
            !    print *,numberOfPegs, test
           !     call jumpBack(&
             !               matrix,&
            !                headExecutedMoves%currentMoveCoordinates%currentCoordinatesStart,&
            !                headExecutedMoves%currentMoveCoordinates%currnetCoordinatesJump,&
          !                  headExecutedMoves%currentMoveCoordinates%currentCoordinatesEnd&
          !             )
         !       call deleteFirstExecutedMoveNode(headExecutedMoves)
          !      call deleteFirstPossibleMoveNode(headExecutedMoves%possibleMoves)
         !       numberOfPegs=numberOfPegs+1
         !       print *, 'currentMatrixTestAfter:'
         !       print *,numberOfPegs, test
      !      end if
        end do
    end subroutine searchSolution

    function reverseArraySize7(array) result(arrayReversed)
        ! Spiegelt ein Array der Groeße 7
        implicit none
        Integer, dimension(1:7)::array
        Integer, dimension(1:7)::arrayReversed
        arrayReversed = array(7:1:-1)
    end function reverseArraySize7

    subroutine rotateMatrix90degree(matrix)
        ! Dreht eine Matrix um 90 Grad
        ! Durch transponieren und rueckwaerts lesen der Zeilen
        implicit none
        integer, dimension(columnLength , rowLength) :: matrix
        integer::n

        matrix=transpose(matrix)
        do n=1,7
            matrix(n, :) = reverseArraySize7(matrix(n, 1:7))
        end do
    end subroutine rotateMatrix90degree

    subroutine addAllUnsolvableCongruentMatricesToList(headUnsolvableMatrix, matrix)
        ! generiert  alle kongruenten Matrizen und fuegt sie der Liste hinzu
        implicit none
        integer, dimension(columnLength , rowLength) :: matrix
        type(listUnsolvablePositions), pointer::headUnsolvableMatrix
        integer::i
        do i=1,4
            call rotateMatrix90degree(matrix)
            call addUnsolvableMatrixToList(headUnsolvableMatrix, matrix)
            call addUnsolvableMatrixToList(headUnsolvableMatrix, transpose(matrix))
        end do
        !call printUnsolvableMatrixList(headUnsolvableMatrix)
    end subroutine addAllUnsolvableCongruentMatricesToList

    subroutine addUnsolvableMatrixToList(headUnsolvableMatrix, CurrentMatrixUnsolvable)
        ! fuegt unloesbare matrix liste hinzu
        implicit none
        integer, dimension(7, 7) :: currentMatrixUnsolvable
        type(listUnsolvablePositions), pointer::headUnsolvableMatrix, tmp, nextUnsolvablePosition

        allocate(tmp)
        tmp%matrixUnsolvable=currentMatrixUnsolvable
        nullify(tmp%nextUnsolvablePosition)
        tmp%nextUnsolvablePosition => headUnsolvableMatrix; headUnsolvableMatrix => tmp
        return
    end subroutine addUnsolvableMatrixToList

    subroutine printUnsolvableMatrixList(head)
        implicit none
        type(listUnsolvablePositions), pointer::tmp
        type(listUnsolvablePositions), pointer::head

        tmp => head
        if(.not.associated(tmp)) then
           print *, 'there are no unsolvable matrices yet'
           return
        else
           print '(A)', ''
           do while(associated(tmp))
              call printMatrix(tmp%matrixUnsolvable)
              tmp => tmp%nextUnsolvablePosition
           end do
        end if
        return
    end subroutine printUnsolvableMatrixList

    function checkIfListContainsMatrix(headUnsolvableMatrix, matrix) result(isIncluded)
        ! Funktion welche prueft, ob aktuelle Matrix in der Liste der unloesbaren Matrizen enthalten ist
        implicit none
        type(listUnsolvablePositions), pointer::tmp
        type(listUnsolvablePositions), pointer::headUnsolvableMatrix
        integer, dimension(7, 7) :: matrix
        logical::isIncluded
        isIncluded = .False.

        tmp => headUnsolvableMatrix
        isIncluded = .False.

           do while(associated(tmp) .And. .Not. isIncluded)
              ! vergleich der Werte von Matrizen
              if(all(tmp%matrixUnsolvable .EQ. matrix)) then
                isIncluded = .True.
                print*, 'unsolvable:'
                call printMatrix(matrix)
                return
              end if
              tmp => tmp%nextUnsolvablePosition
           end do
        return
    end function checkIfListContainsMatrix
end program pegSolitaire2

